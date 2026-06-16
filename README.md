# nes-lter-tdr-bongo

R pipeline for processing temperature and depth profiles from instruments attached to Bongo net tows during Northeast U.S. Shelf Long-Term Ecological Research (NES-LTER) Transect cruises, ongoing since 2018.

Three instrument types are covered: a Star-Oddi DST centi-TD temperature-depth recorder (TDR) deployed on most cruises since 2018; a SeaBird SBE19plus V2 CTD mounted on the Bongo wire for EN668 (summer 2021) and EN706 (summer 2023); and a Kongsberg Simrad PX Universal depth/temperature sensor (SR15 receiver) deployed with the TDR starting with AE2426 (fall 2024).

The processed outputs from this pipeline are published as a data package on the Environmental Data Initiative (EDI) repository. [PLACEHOLDER: DOI]

---

## Repository structure

```
R/
├── 00_helpers.R              helper functions sourced by other scripts
├── 01_tdr_dat_to_csv.R       convert raw .DAT and .xlsx TDR files to CSV
├── 02_tdr_tidy.R             read, clean, label, and QC all TDR data
├── 02_ctd_bongo_tidy.R       read, clean, label, and QC CTD CNV files
├── 02_px_sensor_tidy.R       read, clean, label, and QC PX sensor CSVs
├── 03_tdr_offsets.R          compute TDR-CTD depth offsets
└── 04_instrument_coverage.R  coverage heatmap and summaries
└── 05_qaqc_check.R           quick final QA/QC check 
data/
├── raw/
│   ├── tdr_data/             raw TDR files per cruise (CSV/XLSX/DAT)
│   ├── ctd_bongo/            raw SeaBird CNV files (EN668, EN706)
│   ├── px_sensor/            raw PX sensor CSVs and telemetry XMLs per cruise
│   ├── all-nes-lter-bongologs-YYYYMMDD.csv   bongo logsheet metadata
│   └── elog_zoop_tows_thruXXXX.csv           shipboard event log data for bongos
│   └── nes-lter-zooplankton-tow-metadata-v2.csv    NEED TO ADD DATA PACK LINK
└── processed/
```

> **Note:** Raw data files and figures are not pushed to GitHub. The `data/processed/` outputs are the files submitted to EDI. See the EDI data package for the published versions.

---

## Scripts

Run in this order:

1. **`01_tdr_dat_to_csv.R`** — converts legacy `.DAT` files (EN608, EN627, EN644) and `.xlsx` files to CSV so they can be read by the main pipeline.

2. **`02_tdr_tidy.R`** — main TDR pipeline. cleans and processes all TDR cruise files; exports `tdr_data_no_offset.csv`.

3. **`02_ctd_bongo_tidy.R`** — cleans and processes SeaBird CTD CNV files (EN668, EN706); exports `ctd_bongo_data.csv`.

4. **`02_px_sensor_tidy.R`** — cleans and processes Kongsberg PX sensor files; exports `px_data_bongo.csv`.

5. **`03_tdr_offsets.R`** — computes depth offsets for TDR.

6. **`04_instrument_coverage.R`** — data-instrument availability heatmap (optional).

7. **`05_qaqc_check.R`** —

Helper functions used across scripts are in `R/00_helpers.R`.

---

## Instrument and cruise coverage

### TDR ([Star-Oddi DST centi-TD](https://vocab.nerc.ac.uk/collection/L22/current/TOOL0383/))

Recording interval: 1 second. Two TDR serial numbers used across the time series (9447 and 11871).

| Status | Cruises |
|--------|---------|
| Data available | EN608, EN617, EN627, EN644, EN649, EN655, EN657, AT46, EN687, HRS2303, EN706, AR77, EN712, EN715, EN720, AE2426, EN727, AR88, AR92, AR95, AR99 |
| No data found | EN661, EN695 |
| TDR not deployed | AR63, AR38, AR32 |
| CTD used instead | EN668 |

TDR also not used priot to EN608 (the older OOI cruises maybe list here?)

### CTD ([SeaBird SBE19plus V2 SEACAT](https://vocab.nerc.ac.uk/collection/L22/current/TOOL0871/)), serial no. 8120

Recording interval: 4 Hz (0.25 seconds). Available for EN668 (no TDR) and EN706 (also has TDR).

### PX sensor ([Kongsberg Simrad PX Universal](https://vocab.nerc.ac.uk/collection/L22/current/TOOL1797/)), serial no. 274571

Recording interval: 2 seconds (most cruises); 4 seconds (AR99). Deployed with TDR starting AE2426.

| Cruises with PX data |
|----------------------|
| AE2426, EN727, AR88, AR92, AR95, AR99 |

---

## Data package outputs

The three processed CSV files are the primary outputs submitted to EDI:

| File | Instrument | Columns |
|------|-----------|---------|
| `nes-lter-bongo-tdr.csv` | TDR | cruise, station, cast, date_time, depth_m, temp_C, down_up, note_code, note_detail, tdr_sampling_interval_sec, tdr_max_gap_sec |
| `nes-lter-bongo-ctd.csv` | CTD | cruise, station, cast, date_time, depth_m, temp_C, conductivity_sm, density_kg_m3, descent_rate_ms, down_up, note_code, note_detail |
| `nes-lter-bongo-px.csv`  | PX sensor | cruise, station, cast, date_time, depth_m, temp_C, down_up, note_code, note_detail, px_sampling_interval_sec |

[PLACEHOLDER: NEED TO UPDATE WITH FINAL COLNAMES]

---

## Field protocols

### TDR pre-deployment

- In SeaStar software: Wizards → Connection wizard
- Insert TDR into shuttle; select the COM port that turns green; follow prompts
- First time: go to "Program and Start Recorder" and set measurement interval to 1 second
- Subsequent tows: select "Restart recorder with the same sampling interval" — verify interval before confirming
- Suggested start time is ~6 minutes from now; this is fine
- Insert TDR into rubber sleeve; secure with plastic screw and nut hand-tight
- Insert rubber sleeve into metal sleeve; secure with small metal bolt through wire loop
- Tighten nut until snug against sleeve — no need to overtighten
- Attach to Bongo frame

### TDR offloading

- Remove TDR from metal sleeve by removing the bolt through the wire loop
- In SeaStar: Wizards → Connection Wizard; insert TDR into shuttle; click Yes to retrieve data
- After offload: click Disconnect Recorder in wizard window
- In the plot window: click save (floppy disk icon) — **append to existing filename**, do not overwrite
  - Add cruise number (e.g. EN###), station (e.g. L5), and cast number with B prefix (e.g. B5)
- Save to the appropriate cruise folder in the SeaStar Data folder on the desktop
- Click the Excel export button and save the same way in the same folder
- Close the plot window (not the whole SeaStar window)
- Click Disconnect to put TDR in sleep mode

### PX sensor

The Kongsberg PX Universal D/T sensor is wirelessly deployed on the Bongo net frame. Data are received by the SR15 unit aboard the vessel and logged as semicolon-delimited CSV files with a paired telemetry XML. Files are named by timestamp (YYYYMMDD_HHMMSS_measurements.csv). One file is created per cast.

---

## Dependencies

R packages: `tidyverse`, `here`, `zoo`, `glue`, `lubridate`, `xml2`, `oce`, `conflicted`

External data dependencies (not in this repo):
- Bongo logsheet metadata: `all-nes-lter-bongologs-YYYYMMDD.csv` — compiled in `nes-lter-tow-meta-v3` repo
- Event log data: `elog_zoop_tows_thruXXXX.csv` — compiled in `nes-lter-api-pulls` repo
- NES-LTER REST API2: used in `03_tdr_offsets.R` to fetch shipboard CTD profiles

---

## Related packages and repositories

- NES-LTER zooplankton abundance data package: [knb-lter-nes.25.2] add link 
- NES-LTER zooplankton sample inventory: [knb-lter-nes.24.2] add link
- NES-LTER event logs: [knb-lter-nes.20.2] add link
- Bongo logsheet metadata: [nes-lter-tow-meta-v3](https://github.com/cabanelas/nes-lter-tow-meta-v3)
- Event log data: [nes-lter-api-pulls](https://github.com/cabanelas/nes-lter-api-pulls)

---

## Citation

[PLACEHOLDER: add citation]

## Contact

Alexandra C. Cabanelas — MIT-WHOI Joint Program  