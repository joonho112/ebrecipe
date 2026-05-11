# Discrimination Companion Oracles

This directory is reserved for the numeric oracle assets that reproduce the
Walters (2024) companion discrimination figures.

The source MATLAB CSV files are headerless. Asset-generation code must assign
documented column names explicitly and must verify physical row counts before
writing installed files.

Large grid assets may be serialized as compressed RDS in Step 2.2. The
version-specific manifest in `../registry/` records both the source digest and
the installed digest.
