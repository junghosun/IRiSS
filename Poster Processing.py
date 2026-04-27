"""
Poster Color Extraction and ΔE 2000 Computation Pipeline
=========================================================

This pipeline processes Korean campaign posters from elections 2000-2022.
For each poster image, it:
  1. Extracts the 3 most dominant colors using K-means clustering
  2. Records the RGB values and area percentage of each cluster
  3. Computes the CIE ΔE 2000 distance between the dominant color and 
     the party's official registered color
  4. Merges with candidate-level vote data
  5. Builds the master analysis dataset

INPUT:
  - Poster image files (.jpg or .png), organized by election folder
  - Candidate vote data CSV files (one per election-party)
  - Official party color records

OUTPUT:
  - {election}_{party}_color_with_vote_data.csv (one per election-party)
  - master_data.csv (combined dataset)

DEPENDENCIES:
  pip install pillow numpy scikit-learn scikit-image pandas tqdm
"""

import os
import re
from pathlib import Path
from typing import Tuple, List, Dict

import numpy as np
import pandas as pd
from PIL import Image
from sklearn.cluster import KMeans
from skimage.color import rgb2lab, deltaE_ciede2000
from tqdm import tqdm


# =============================================================================
# CONFIGURATION
# =============================================================================

# Root directory containing poster images organized as:
#   POSTER_ROOT/{election_id}/{party_name}/{candidate_name}.jpg
POSTER_ROOT = "/path/to/poster/images"

# Directory containing per-election-party vote CSV files
VOTE_DATA_DIR = "/path/to/vote_data"

# Output directory
OUTPUT_DIR = "/path/to/output"

# K-means parameters
N_CLUSTERS = 3              # Extract 3 dominant colors per poster
KMEANS_RANDOM_STATE = 42    # For reproducibility
KMEANS_N_INIT = 10          # Number of K-means initializations
RESIZE_DIM = 200            # Resize images to this max dimension before clustering
                            # (speeds up computation; clustering is robust to this)

# Official party colors (RGB), keyed by election_party_name combination
# These are the colors registered with the National Election Commission
# at the time of each election
PARTY_COLORS = {
    # Legislative elections
    "21대_총선_더불어민주당":     (0,    78,   161),
    "21대_총선_미래통합당":      (239,  66,   111),
    "20대_총선_더불어민주당":     (0,    78,   161),
    "20대_총선_새누리당":        (201,  37,    43),
    "20대_총선_국민의당":        (101,  161,   50),
    "19대_총선_민주통합당":      (255,  213,    4),
    "19대_총선_새누리당":        (201,  37,    43),
    "18대_총선_통합민주당":      (65,   150,   57),
    "18대_총선_한나라당":        (0,    149,  218),
    "17대_총선_열린우리당":      (255,  217,   24),
    "17대_총선_한나라당":        (0,    149,  218),
    "16대_총선_한나라당":        (0,    0,    168),
    "16대_총선_새천년민주당":    (0,    170,  123),
    
    # Local elections — 광역 (metropolitan/provincial)
    "8대_지선_광역_더불어민주당":  (0,    78,   161),
    "8대_지선_광역_국민의힘":     (230,  30,    43),
    "7대_지선_광역_더불어민주당":  (0,    78,   161),
    "7대_지선_광역_자유한국당":   (201,  21,    30),
    "6대_지선_광역_새정치민주연합": (0,    130,  205),
    "6대_지선_광역_새누리당":    (201,  37,    43),
    "5대_지선_광역_민주당":      (1,    158,   51),
    "5대_지선_광역_한나라당":    (0,    149,  218),
    "4대_지선_광역_열린우리당":   (255,  217,   24),
    "4대_지선_광역_한나라당":    (0,    149,  218),
    "4대_지선_광역_민주당":      (0,    170,  123),
    "3대_지선_광역_새천년민주당": (0,    170,  123),
    "3대_지선_광역_한나라당":    (0,    0,    168),
    
    # Local elections — 기초 (basic/local)
    "8대_지선_기초_더불어민주당":  (0,    78,   161),
    "8대_지선_기초_국민의힘":     (230,  30,    43),
    "7대_지선_기초_더불어민주당":  (0,    78,   161),
    "7대_지선_기초_자유한국당":   (201,  21,    30),
    "6대_지선_기초_새정치민주연합": (0,    130,  205),
    "6대_지선_기초_새누리당":    (201,  37,    43),
    "5대_지선_기초_민주당":      (1,    158,   51),
    "5대_지선_기초_한나라당":    (0,    149,  218),
    "4대_지선_기초_열린우리당":   (255,  217,   24),
    "4대_지선_기초_한나라당":    (0,    149,  218),
    "4대_지선_기초_민주당":      (0,    170,  123),
    "3대_지선_기초_새천년민주당": (0,    170,  123),
    "3대_지선_기초_한나라당":    (0,    0,    168),
}


# =============================================================================
# COLOR EXTRACTION FUNCTIONS
# =============================================================================

def load_and_resize_image(image_path: str, max_dim: int = RESIZE_DIM) -> np.ndarray:
    """
    Load an image and resize it for efficient K-means processing.
    Returns RGB pixel array of shape (H, W, 3).
    """
    img = Image.open(image_path).convert('RGB')
    
    # Resize if larger than max_dim, preserving aspect ratio
    w, h = img.size
    if max(w, h) > max_dim:
        scale = max_dim / max(w, h)
        new_w = int(w * scale)
        new_h = int(h * scale)
        img = img.resize((new_w, new_h), Image.LANCZOS)
    
    return np.array(img)


def extract_dominant_colors(
    image_array: np.ndarray, 
    n_clusters: int = N_CLUSTERS
) -> Tuple[List[Tuple[int, int, int]], List[float]]:
    """
    Extract the n_clusters most dominant colors from an image using K-means.
    
    Returns:
        colors: list of (R, G, B) tuples, sorted by area descending
        percentages: list of area percentages (0-100), corresponding to colors
    """
    # Reshape to (n_pixels, 3) for K-means
    pixels = image_array.reshape(-1, 3)
    
    # Run K-means clustering
    kmeans = KMeans(
        n_clusters=n_clusters,
        random_state=KMEANS_RANDOM_STATE,
        n_init=KMEANS_N_INIT
    )
    labels = kmeans.fit_predict(pixels)
    centers = kmeans.cluster_centers_.astype(int)
    
    # Calculate the percentage of pixels in each cluster
    label_counts = np.bincount(labels, minlength=n_clusters)
    total_pixels = len(pixels)
    percentages = (label_counts / total_pixels) * 100
    
    # Sort clusters by area (descending)
    order = np.argsort(-percentages)
    sorted_colors = [tuple(int(c) for c in centers[i]) for i in order]
    sorted_pcts = [float(percentages[i]) for i in order]
    
    return sorted_colors, sorted_pcts


def compute_delta_e_2000(rgb1: Tuple[int, int, int], 
                         rgb2: Tuple[int, int, int]) -> float:
    """
    Compute the CIE ΔE 2000 perceptual color distance between two RGB colors.
    Uses scikit-image's deltaE_ciede2000 function, which is the industrial
    standard implementation.
    
    Returns:
        ΔE 2000 value (typically ranges from 0 to 100)
        - 0:    identical colors
        - 1-2:  barely perceptible difference
        - 5:    clearly perceptible difference
        - 25+:  perceptually distinct colors
    """
    # Convert RGB [0, 255] to RGB [0, 1] for skimage
    rgb1_norm = np.array(rgb1, dtype=float).reshape(1, 1, 3) / 255.0
    rgb2_norm = np.array(rgb2, dtype=float).reshape(1, 1, 3) / 255.0
    
    # Convert RGB to CIE L*a*b*
    lab1 = rgb2lab(rgb1_norm)
    lab2 = rgb2lab(rgb2_norm)
    
    # Compute ΔE 2000
    delta = deltaE_ciede2000(lab1, lab2)
    return float(delta[0, 0])


# =============================================================================
# POSTER PROCESSING PIPELINE
# =============================================================================

def process_single_poster(
    image_path: str,
    party_color: Tuple[int, int, int]
) -> Dict:
    """
    Process a single poster image and return all measurements.
    
    Returns a dict with:
        - dominant_color_1, _2, _3: RGB tuples
        - percentage_1, _2, _3: area percentages
        - delta_e_1, _2, _3: ΔE 2000 to party color for each cluster
        - delta_e: primary measure (ΔE of dominant color)
        - delta_e_weighted: area-weighted average ΔE across 3 clusters
        - delta_e_min: minimum ΔE across 3 clusters
    """
    # Load and process image
    image_array = load_and_resize_image(image_path)
    colors, pcts = extract_dominant_colors(image_array)
    
    # Compute ΔE 2000 for each cluster
    deltas = [compute_delta_e_2000(c, party_color) for c in colors]
    
    result = {}
    
    # Store each cluster's color, percentage, and ΔE
    for i, (color, pct, delta) in enumerate(zip(colors, pcts, deltas), start=1):
        result[f'Color Code {i}'] = f"({color[0]}, {color[1]}, {color[2]})"
        result[f'Percentage {i}'] = round(pct, 2)
        result[f'delta_e_{i}'] = round(delta, 4)
    
    # Primary measure: ΔE of dominant color
    result['delta_e'] = round(deltas[0], 4)
    
    # Weighted average (used in robustness check)
    total_pct = sum(pcts)
    if total_pct > 0:
        weighted = sum(d * p for d, p in zip(deltas, pcts)) / total_pct
        result['delta_e_weighted'] = round(weighted, 4)
    else:
        result['delta_e_weighted'] = np.nan
    
    # Minimum across 3 clusters (alternative measure)
    result['delta_e_min'] = round(min(deltas), 4)
    
    return result


def process_election_party(
    election_id: str,
    party_name: str,
    poster_dir: str,
    vote_csv_path: str,
    output_csv_path: str
) -> pd.DataFrame:
    """
    Process all posters for a single election-party combination.
    Merges color extraction results with candidate vote data.
    
    Args:
        election_id: e.g., "21대_총선" or "8대_지선_광역"
        party_name: e.g., "더불어민주당"
        poster_dir: directory containing this party's poster images
        vote_csv_path: path to vote data CSV with columns including:
                       후보자명, 정당명, 득표율, 득표율 차, 당선여부, 선거구
        output_csv_path: where to save the merged result
    """
    # Look up party color
    party_key = f"{election_id}_{party_name}"
    if party_key not in PARTY_COLORS:
        raise KeyError(f"Party color not registered for: {party_key}")
    
    party_color = PARTY_COLORS[party_key]
    
    # Load vote data
    vote_df = pd.read_csv(vote_csv_path, encoding='utf-8')
    
    # Process each poster
    results = []
    poster_dir_path = Path(poster_dir)
    
    for _, row in tqdm(vote_df.iterrows(), 
                        total=len(vote_df),
                        desc=f"{party_key}"):
        candidate_name = row['후보자명']
        
        # Find this candidate's poster file
        # File could be .jpg or .png
        poster_file = None
        for ext in ['.jpg', '.jpeg', '.png']:
            candidate_path = poster_dir_path / f"{candidate_name}{ext}"
            if candidate_path.exists():
                poster_file = candidate_path
                break
        
        if poster_file is None:
            print(f"  Warning: poster not found for {candidate_name}")
            continue
        
        # Process the poster
        try:
            color_data = process_single_poster(str(poster_file), party_color)
        except Exception as e:
            print(f"  Error processing {candidate_name}: {e}")
            continue
        
        # Combine candidate info with color data
        combined = {**row.to_dict(), **color_data}
        results.append(combined)
    
    # Build DataFrame and save
    result_df = pd.DataFrame(results)
    result_df.to_csv(output_csv_path, index=False, encoding='utf-8-sig')
    
    print(f"  Saved {len(result_df)} candidates to {output_csv_path}")
    return result_df


# =============================================================================
# MASTER DATASET BUILDER
# =============================================================================

# Mapping from election prefix to year and election type
YEAR_MAP = {
    "16대": 2000, "17대": 2004, "18대": 2008, "19대": 2012,
    "20대": 2016, "21대": 2020,
    "3대":  2002, "4대":  2006, "5대":  2010,
    "6대":  2014, "7대":  2018, "8대":  2022,
}

# Mapping from party name to camp
CAMP_MAP = {
    "더불어민주당":   "Progressive",
    "미래통합당":     "Conservative",
    "새누리당":       "Conservative",
    "국민의당":       "Third",
    "민주통합당":     "Progressive",
    "통합민주당":     "Progressive",
    "한나라당":       "Conservative",
    "열린우리당":     "Progressive",
    "새천년민주당":   "Progressive",
    "자유한국당":     "Conservative",
    "국민의힘":       "Conservative",
    "민주당":         "Progressive",
    "새정치민주연합": "Progressive",
}


def parse_filename_metadata(filename: str) -> Dict:
    """
    Parse election metadata from a filename like:
    '21대_총선_더불어민주당_color_with_vote_data.csv'
    or
    '8대_지선_광역_더불어민주당_color_with_vote_data.csv'
    """
    stem = re.sub(r'^\d+_', '', filename)  # remove any numeric prefix
    stem = stem.replace('_color_with_vote_data.csv', '')
    
    parts = stem.split('_')
    
    if '총선' in parts:
        # Legislative: e.g., "21대_총선_더불어민주당"
        enum = parts[0]
        etype = "National"
        party = parts[2]
    elif '지선' in parts:
        # Local: e.g., "8대_지선_광역_더불어민주당"
        enum = parts[0]
        etype = "Local"
        party = parts[3]
    else:
        return None
    
    return {
        'enum': enum,
        'etype': etype,
        'party': party,
        'year': YEAR_MAP.get(enum),
        'camp': CAMP_MAP.get(party, 'Unknown'),
        'source': stem,
    }


def build_master_dataset(
    output_dir: str,
    master_path: str = None
) -> pd.DataFrame:
    """
    Combine all per-election-party CSV files into a single master dataset.
    Adds standardized indicator variables used in the analysis.
    """
    if master_path is None:
        master_path = os.path.join(output_dir, 'master_data.csv')
    
    csv_files = sorted([
        f for f in os.listdir(output_dir)
        if f.endswith('_color_with_vote_data.csv')
    ])
    
    print(f"Found {len(csv_files)} per-election-party CSV files")
    
    all_rows = []
    for filename in csv_files:
        meta = parse_filename_metadata(filename)
        if meta is None:
            print(f"  Skipping (cannot parse): {filename}")
            continue
        
        df = pd.read_csv(os.path.join(output_dir, filename), encoding='utf-8-sig')
        if len(df) == 0:
            continue
        
        # Add metadata columns
        for k, v in meta.items():
            df[k] = v
        
        all_rows.append(df)
    
    master = pd.concat(all_rows, ignore_index=True)
    
    # Add indicator variables for analysis
    master['is_national']     = (master['etype'] == 'National').astype(int)
    master['is_local']        = (master['etype'] == 'Local').astype(int)
    master['is_conservative'] = (master['camp']  == 'Conservative').astype(int)
    master['is_progressive']  = (master['camp']  == 'Progressive').astype(int)
    master['is_third']        = (master['camp']  == 'Third').astype(int)
    master['is_winner']       = (master['당선여부'] == '당선').astype(int)
    master['post2017']        = (master['year']   >= 2018).astype(int)
    
    # Drop rows with missing ΔE
    before = len(master)
    master = master[master['delta_e'].notna()].copy()
    after = len(master)
    if before != after:
        print(f"Dropped {before - after} rows with missing ΔE")
    
    # Save
    master.to_csv(master_path, index=False, encoding='utf-8-sig')
    print(f"Master dataset saved: {master_path}")
    print(f"  N = {len(master)}")
    print(f"  By arena: {master['etype'].value_counts().to_dict()}")
    print(f"  By camp:  {master['camp'].value_counts().to_dict()}")
    
    return master


# =============================================================================
# MAIN EXECUTION
# =============================================================================

def main():
    """
    Example top-level pipeline. Customize paths to your local setup.
    """
    os.makedirs(OUTPUT_DIR, exist_ok=True)
    
    # ---- Step 1: Process each election-party's posters ----
    # 
    # You'll need to customize this loop based on how your poster images
    # and vote CSVs are organized on disk. The expected structure is:
    #
    #   POSTER_ROOT/
    #     21대_총선/
    #       더불어민주당/
    #         홍길동.jpg
    #         김철수.jpg
    #         ...
    #
    #   VOTE_DATA_DIR/
    #     21대_총선_더불어민주당.csv  (with cols: 후보자명, 득표율, 당선여부, ...)
    #     21대_총선_미래통합당.csv
    #     ...
    
    print("=" * 70)
    print("STEP 1: Processing posters and computing ΔE 2000")
    print("=" * 70)
    
    for party_key in PARTY_COLORS.keys():
        # Parse election_id and party_name from key
        # e.g., "21대_총선_더불어민주당" -> election="21대_총선", party="더불어민주당"
        # e.g., "8대_지선_광역_더불어민주당" -> election="8대_지선_광역", party="더불어민주당"
        if '총선' in party_key:
            election_id = '_'.join(party_key.split('_')[:2])
            party_name  = '_'.join(party_key.split('_')[2:])
        else:  # 지선
            election_id = '_'.join(party_key.split('_')[:3])
            party_name  = '_'.join(party_key.split('_')[3:])
        
        poster_dir   = os.path.join(POSTER_ROOT, election_id, party_name)
        vote_csv     = os.path.join(VOTE_DATA_DIR, f"{party_key}.csv")
        output_csv   = os.path.join(OUTPUT_DIR,   f"{party_key}_color_with_vote_data.csv")
        
        if not os.path.exists(poster_dir):
            print(f"  Skipping {party_key}: poster directory not found")
            continue
        if not os.path.exists(vote_csv):
            print(f"  Skipping {party_key}: vote CSV not found")
            continue
        if os.path.exists(output_csv):
            print(f"  Already processed: {party_key}")
            continue
        
        process_election_party(
            election_id=election_id,
            party_name=party_name,
            poster_dir=poster_dir,
            vote_csv_path=vote_csv,
            output_csv_path=output_csv,
        )
    
    # ---- Step 2: Build master dataset ----
    print("\n" + "=" * 70)
    print("STEP 2: Building master dataset")
    print("=" * 70)
    
    master = build_master_dataset(OUTPUT_DIR)
    
    print("\nDone. Master dataset is ready for regression analysis.")
    return master


if __name__ == "__main__":
    main()
