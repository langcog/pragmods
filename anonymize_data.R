# Create the public, de-identified data/ folder from the raw MTurk files in
# unanon_data/ (which is not shared).
#
# For each participant-level file:
#   * the MTurk worker ID is replaced by an anonymous ID ("S" + 5 digits). The
#     mapping is shared across ALL files, so a person who took part in several
#     experiments has the same anonymous ID everywhere (this is what the
#     analyses use to exclude repeat participants).
#   * columns that identify an individual assignment (assignment/HIT IDs and
#     per-assignment timestamps) are removed.
#   * files are written as UTF-8 with LF line endings, all fields quoted, with
#     the original delimiter.
# Non-participant files (experiment/condition tables) are copied unchanged.
#
# Some files already had their worker IDs replaced by within-file row numbers
# (1, 2, 3, ...). These rows are keyed on an assignment fingerprint (HIT ID +
# accept and submit times) instead, so copies of the same file in different
# folders still get the same anonymous IDs.
#
# The worker ID -> anonymous ID key is saved to unanon_data/anon_key.csv (NOT
# shared) and reused on re-runs so that IDs are stable.
#
# Run from the repository root: Rscript anonymize_data.R

in_dir  <- "unanon_data"
out_dir <- "data"
key_file <- file.path(in_dir, "anon_key.csv")

# columns (lower-cased) removed from every file
drop_cols <- c("hitid", "hittypeid", "viewhit", "assignmentid",
               "assignmentaccepttime", "assignmentsubmittime",
               "assignmentapprovaltime", "assignmentrejecttime",
               "autoapprovaltime", "feedback", "reject",
               "accepttime", "submittime")

# read a delimited file of unknown encoding / line endings / delimiter.
# all values are read as character, exactly as they appear in the file.
read_raw <- function(f) {
  txt <- rawToChar(readBin(f, "raw", file.info(f)$size))
  Encoding(txt) <- if (validUTF8(txt)) "UTF-8" else "latin1"
  txt <- gsub("\r\n?", "\n", enc2utf8(txt))
  first <- sub("\n.*", "", txt)
  sep <- if (grepl("\t", first)) "\t" else ","
  n_fields <- max(count.fields(textConnection(txt), sep = sep, quote = "\"",
                               comment.char = "", blank.lines.skip = TRUE), na.rm = TRUE)
  d <- read.table(text = txt, header = FALSE, sep = sep, quote = "\"",
                  comment.char = "", colClasses = "character",
                  na.strings = character(0), fill = TRUE,
                  col.names = paste0("V", 1:n_fields), encoding = "UTF-8")
  header <- unlist(d[1, ])
  d <- d[-1, , drop = FALSE]
  # old-style MTurk exports end every data row with a trailing delimiter,
  # giving an extra, empty, unnamed field
  extra <- header == ""
  stopifnot(all(unlist(d[, extra]) == ""))
  d <- d[, !extra, drop = FALSE]
  names(d) <- header[!extra]
  rownames(d) <- NULL
  list(d = d, sep = sep)
}

files <- list.files(in_dir, pattern = "\\.(csv|tsv)$", recursive = TRUE)
files <- setdiff(files, basename(key_file))
dat <- setNames(lapply(file.path(in_dir, files), read_raw), files)

id_col <- function(d) grep("^workerid$", names(d), ignore.case = TRUE, value = TRUE)
is_subj <- sapply(dat, function(x) length(id_col(x$d)) == 1)
cat("Participant files:", sum(is_subj), " Other files:", sum(!is_subj), "\n")
print(files[!is_subj])

# --- raw key for every row -------------------------------------------------
row_keys <- function(d) {
  ids <- trimws(d[[id_col(d)]])
  if (all(grepl("^A[0-9A-Z]{8,20}$", ids))) return(ids)  # real MTurk worker IDs
  stopifnot(all(grepl("^[0-9]+$", ids)))                  # within-file row numbers
  nm <- tolower(names(d))
  fp <- paste(d[[which(nm == "hitid")]], d[[which(nm == "assignmentaccepttime")]],
              d[[which(nm == "assignmentsubmittime")]], sep = "|")
  stopifnot(!anyDuplicated(fp), all(nchar(fp) > 10))
  paste0("fingerprint:", fp)
}
keys <- lapply(dat[is_subj], function(x) row_keys(x$d))

# --- build / extend the key -------------------------------------------------
all_keys <- unique(unlist(keys))
key <- if (file.exists(key_file)) {
  read.csv(key_file, colClasses = "character")
} else {
  data.frame(raw_key = character(0), anon_id = character(0))
}
new <- setdiff(all_keys, key$raw_key)
if (length(new) > 0) {
  used <- as.integer(sub("^S", "", key$anon_id))
  pool <- setdiff(1:99999, used)
  key <- rbind(key, data.frame(raw_key = new,
                               anon_id = sprintf("S%05d", sample(pool, length(new)))))
  write.csv(key, key_file, row.names = FALSE)
}
stopifnot(!anyDuplicated(key$raw_key), !anyDuplicated(key$anon_id))
cat("Unique participants (raw keys):", length(all_keys),
    " of which fingerprint-keyed:", sum(grepl("^fingerprint:", all_keys)), "\n")

# --- write ------------------------------------------------------------------
unlink(list.files(out_dir, pattern = "\\.(csv|tsv)$", recursive = TRUE, full.names = TRUE))
out_path <- function(f) file.path(out_dir, gsub("unused /", "unused/", f, fixed = TRUE))

for (f in files[!is_subj]) {  # copied unchanged
  dir.create(dirname(out_path(f)), recursive = TRUE, showWarnings = FALSE)
  file.copy(file.path(in_dir, f), out_path(f))
}

for (f in files[is_subj]) {
  d <- dat[[f]]$d
  idc <- id_col(d)
  d[[idc]] <- key$anon_id[match(keys[[f]], key$raw_key)]
  stopifnot(!anyNA(d[[idc]]))
  d <- d[, !(tolower(names(d)) %in% drop_cols), drop = FALSE]
  dir.create(dirname(out_path(f)), recursive = TRUE, showWarnings = FALSE)
  write.table(d, out_path(f), sep = dat[[f]]$sep, quote = TRUE, qmethod = "double",
              row.names = FALSE, na = "NA", fileEncoding = "UTF-8", eol = "\n")
}
cat("Wrote", length(files), "files to", out_dir, "\n")
