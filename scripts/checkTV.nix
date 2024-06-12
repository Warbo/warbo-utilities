{
  bash,
  cacert,
  openssl,
  raw,
  wget,
  wrap,
}:

wrap {
  name = "checkTV";
  vars = {
    SSL_CERT_FILE = "${cacert}/etc/ssl/certs/ca-bundle.crt";
  };
  paths = [
    bash
    openssl
    (openssl.dev or openssl)
    wget
  ];
  file = raw."checkTV.sh";
}
