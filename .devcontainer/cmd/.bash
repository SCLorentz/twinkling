# Alias para ls -la
alias ll='ls -la'

# Mudar a cor do prompt (para Bash, usando cores ANSI)
PS1='\[\e[38;5;41m\]\w\[\e[0m\]\\$ '

run() {
    sbcl --script ./.test/hello-world.lsp 
}

export SYSTEM_DEPS_PANGO_NO_PKG_CONFIG="true"
export SYSTEM_DEPS_PANGO_LIB="pango-1.0"