{
  bash,
  git,
  raw,
  wrap,
}:

wrap {
  name = "git-migrate-file";
  file = raw.git-migrate-file;
  paths = [
    bash
    git
  ];
}
