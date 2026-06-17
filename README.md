# nes-lter-tdr-bongo

R pipeline for processing temperature and depth profiles from instruments attached to Bongo net tows during *Northeast U.S. Shelf Long-Term Ecological Research (NES-LTER) Transect cruises*, ongoing since 2018.

Three instrument types are covered: 
* Star-Oddi DST centi-TD temperature-depth recorder (TDR) deployed on most cruises since 2018
* SeaBird SBE19plus V2 CTD mounted on the Bongo wire for EN668 (summer 2021) and EN706 (summer 2023)
* Kongsberg Simrad PX Universal depth/temperature sensor (SR15 receiver) deployed with the TDR starting with AE2426 (fall 2024)

The processed outputs from this pipeline are published as a data package on the Environmental Data Initiative (EDI) repository. [PLACEHOLDER: DOI] This is an ongoing data package: data currently span 2018-2026, and the package will be updated as additional NES-LTER transect cruises are completed.

---

## Repository structure

```
R/
├── 00_helpers.R              helper functions sourced by other scripts
├── 01_tdr_dat_to_csv.R       convert raw .DAT and .xlsx TDR files to CSV
├── 02_tdr_tidy.R             read, clean, label, and QC all TDR data
├── 02_ctd_bongo_tidy.R       read, clean, label, and QC CTD CNV files
├── 02_px_sensor_tidy.R       read, clean, label, and QC PX sensor CSVs
├── 03_tdr_offsets.R          compute TDR depth offsets
└── 04_instrument_coverage.R  coverage heatmap and summaries
└── 05_qaqc_check.R           final structural QA/QC 
data/
├── raw/
│   ├── tdr_data/             raw TDR files per cruise (CSV/XLSX/DAT)
│   ├── ctd_bongo/            raw SeaBird CNV files (EN668, EN706)
│   ├── px_sensor/            raw PX sensor CSVs and telemetry XMLs per cruise
│   ├── all-nes-lter-bongologs-YYYYMMDD.csv   bongo logsheet metadata
│   └── elog_zoop_tows_thruXXXX.csv           shipboard event log data for bongos
│   └── tdr_offsets.csv           manually curated TDR offset notes (input to 03_tdr_offsets.R)
└── processed/
```

> **Note:** Raw instrument data files and saved figures are not pushed to GitHub. The `data/processed/` outputs are the files submitted to EDI. See the EDI data package for the published versions. 

---

## Scripts

Run in this order:

1. **`01_tdr_dat_to_csv.R`** — converts `.DAT` files (EN608, EN627, EN644) and `.xlsx` files to `.csv` so they can be read by the main pipeline.

2. **`02_tdr_tidy.R`** — main TDR pipeline. cleans and processes all TDR data; exports `nes-lter-bongo-tdr.csv`.

3. **`02_ctd_bongo_tidy.R`** — cleans and processes SeaBird CTD CNV files (EN668, EN706); exports `nes-lter-bongo-ctd.csv`.

4. **`02_px_sensor_tidy.R`** — cleans and processes Kongsberg PX sensor files; exports `nes-lter-bongo-px.csv`.

5. **`03_tdr_offsets.R`** — computes depth offsets for TDR; exports .

6. **`04_instrument_coverage.R`** — data-instrument availability heatmap (optional).

7. **`05_qaqc_check.R`** — final QA/QC 

Helper functions used across scripts are in `R/00_helpers.R`.

---

## Instrument and cruise coverage

### TDR ([Star-Oddi DST centi-TD](https://vocab.nerc.ac.uk/collection/L22/current/TOOL0383/))

Recording interval: nominally 1 second (actual interval per deployment is in the `sampling_interval_sec` column). Three TDR serial numbers used across the time series: 9447 (2018–2023), 9446 (EN644, EN687), and 11871 (2023–2026).

| Status | Cruises |
|--------|---------|
| Data available | EN608, EN617, EN627, EN644, EN649, EN655, EN657, AT46, EN687, HRS2303, EN706, AR77, EN712, EN715, EN720, AE2426, EN727, AR88, AR92, AR95, AR99 |
| No TDR data found | EN661, EN695 |
| TDR not deployed | AR63, AR38, AR32 |
| CTD used instead | EN668 |

Prior to EN608, the first dedicated NES-LTER transect cruise, zooplankton sampling consisted of vertical ring net tows (AR28B, AR31A, AR34B, AR39B, AR61B, AR66B) conducted in collaboration with OOI. No Bongo tows or TDR data exist for these cruises, zooplankton samples from the ring net tows are available.

### CTD ([SeaBird SBE19plus V2 SEACAT](https://vocab.nerc.ac.uk/collection/L22/current/TOOL0871/)), serial no. 8120

Recording interval: 4 Hz (0.25 seconds). Available for EN668 (no TDR) and EN706 (also has TDR).

### PX sensor ([Kongsberg Simrad PX Universal](https://vocab.nerc.ac.uk/collection/L22/current/TOOL1797/)), serial no. 274571

Recording interval: 2 seconds (most cruises); 4 seconds (AR99). Deployed with TDR starting cruise AE2426 (2024).

| Cruises with PX data |
|----------------------|
| AE2426, EN727, AR88, AR92, AR95, AR99 |

---

## Data package outputs

Four processed CSV files are the primary outputs submitted to EDI:

| File | Instrument | Columns |
|------|-----------|---------|
| `nes-lter-bongo-tdr.csv` | TDR | cruise, station, cast, date_time, depth_m, temp_C, down_up, note_code, note_detail, serial_number, lifetime_cast, seastar_version, sampling_interval_sec, max_gap_sec, n_obs |
| `nes-lter-bongo-ctd.csv` | CTD | cruise, station, cast, date_time, depth_m, temp_C, down_up, note_code, note_detail, file_start_time, conductivity_S_m, density_kg_m3, descent_rate_m_s, elapsed_s |
| `nes-lter-bongo-px.csv` | PX sensor | cruise, station, cast, date_time, depth_m, temp_C, down_up, note_code, note_detail, file_start_time, sampling_interval_sec, max_gap_sec, n_obs |
| `nes-lter-bongo-tdr-offsets.csv` | TDR offsets | cruise, station, cast, tdr_max_depth_m, px_max_depth_m, ctd_bongo_max_depth_m, offset_m |

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

R packages: `tidyverse`, `here`, `zoo`, `glue`, `lubridate`, `xml2`, `oce`, `conflicted`, `plotly`, `readxl`, `openxlsx`

External data dependencies (not in this repo):
- Bongo logsheet metadata: `all-nes-lter-bongologs-YYYYMMDD.csv` — compiled in `nes-lter-tow-meta-v3` repo. Extends the published [NES-LTER zooplankton tow metadata package](https://doi.org/10.6073/pasta/8ff3d6baebd5e10cf59c527da0081e4b) with cruises completed after that package's last update, needed here for processing the more recent TDR cruises. 
- Event log data: `elog_zoop_tows_thruXXXX.csv` — compiled in `nes-lter-api-pulls` repo
- NES-LTER REST API2: used in `03_tdr_offsets.R` to fetch shipboard CTD profiles

---

## Related packages and repositories

- NES-LTER zooplankton abundance data package: [knb-lter-nes.25.2](https://doi.org/10.6073/pasta/15ef526d7e6c92ba551d31327625654c)
- NES-LTER zooplankton sample inventory: [knb-lter-nes.24.2](https://doi.org/10.6073/pasta/8ff3d6baebd5e10cf59c527da0081e4b)
- NES-LTER event logs: [knb-lter-nes.20.2](https://doi.org/10.6073/pasta/0cde75ba26923d87e107a1c440613209)
- Bongo logsheet metadata: [nes-lter-tow-meta-v3](https://github.com/cabanelas/nes-lter-tow-meta-v3)
- Event log data: [nes-lter-api-pulls](https://github.com/cabanelas/nes-lter-api-pulls)

---

## Citation

[PLACEHOLDER: add citation]

## Contact

Alexandra C. Cabanelas — MIT-WHOI Joint Program  