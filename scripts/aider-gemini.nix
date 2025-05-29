{ aider-chat, writeShellScript }:
writeShellScript "aider-gemini.sh" ''
  export GEMINI_API_KEY
  GEMINI_API_KEY=$(pass web/gg/gemini)
  exec ${aider-chat}/bin/aider "$@"
''
