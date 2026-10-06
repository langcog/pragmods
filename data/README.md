# Data

De-identified Amazon Mechanical Turk data for the pragmods experiments (2013–2015). Each file is one MTurk batch; each row is one participant. Folders match the analyses in `analysis/` (`1-prelims`, `2-prior`, `3-levels`, `4-sequences`, `5-speakers`). Files in `unused/` (and `4-sequences/originals/`, `3-levels/size/unused-size/`) are pilots, partial batches, earlier exports, or alternate versions that are not read by the analysis code. They are included for completeness.

`all_experiments.csv` is a log of the experiments and their conditions. `3-levels/size/scale_and_level.csv` describes the stimulus matrices for the size experiment.

## De-identification

These files were created from the raw MTurk results by `anonymize_data.R` (the raw files are not shared). For each participant file:

- **`workerid` holds an anonymous participant ID** (`S` + 5 digits) instead of the MTurk worker ID. One ID mapping covers all files, so a person who took part in more than one experiment has the same ID everywhere. The analyses rely on this to exclude repeat participants.
- **Columns that identify an individual assignment were removed:** `hitid`, `hittypeid`, `viewhit`, `assignmentid`, `assignmentaccepttime`, `assignmentsubmittime`, `assignmentapprovaltime`, `assignmentrejecttime`, `autoapprovaltime`, `feedback`, `reject` (and `HitId`, `AssignmentId`, `AcceptTime`, `SubmitTime` in the older `Answer 1…N` export format).
- All other columns, including HIT-level metadata (title, reward, creation date) and participants' free-text comments, are unchanged. The free-text fields were screened for worker IDs, emails, URLs, IP addresses, phone numbers, and self-identifying statements.
- Files are re-written as UTF-8 with LF line endings and all fields quoted. The original delimiter is kept (note that some `.csv` files in `unused/` are actually tab-separated).

Notes:

- The `4-sequences` files and `5-speakers/pragmods_overspec_baseline.results.csv` had already had their worker IDs replaced by within-file row numbers before this step. Their participants therefore get unique anonymous IDs but cannot be linked to other experiments. Copies of the same batch (e.g. `4-sequences/originals/`) share IDs.
- A few responses in the three `5-speakers/*virtual*` files contain characters that were garbled by the virtual keyboard at collection time. They are preserved as-is.
- The folder `1-prelims/unused /` (trailing space) was renamed to `1-prelims/unused/`.
