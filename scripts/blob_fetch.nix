{
  bash,
  curl,
  raw,
  wrap,
}:

wrap {
  name = "blob_fetch";
  file = raw.blob_fetch;
  paths = [
    bash
    curl
  ];
}
