# Pre-release changes/updates

# dpm.dashboard 0.2.0

## Modified available dashboard

Removed bloat dashboards, keeping an extended single dashboard (launch_dashboard()) for running demo and sensitivity analysis.

Restructured the package to fix bugs when installing the package rather than loading package functionality. 

# dpm.dashboard 0.1.0

## Added a demo dashboard

Added a new demo dashboard (run_demo_dash()) with reduced functionality, and default paths for demo data. Simple format - running the user through adding the system models (defaults provided) and then adding the data models (no default paths, but the data is available in the data folder)

## Separated dashboards

`run_full_dash()` - Launch the full DPM dashboard with single/multi-compare tabs.

`run_dev_dash()` - Launch the developmental dashboard with additional system model setups.

# dpm.dashboard 0.0.1

## Initial dashboard setup

`run_dpm_dashboard()` - Launch the full DPM dashboard with single/multi-compare tabs.
