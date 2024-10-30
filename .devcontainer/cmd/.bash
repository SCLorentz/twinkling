# Alias para ls -la
alias ll='ls -la'

# Mudar a cor do prompt (para Bash, usando cores ANSI)
PS1='\[\e[38;5;41m\]\w\[\e[0m\]\\$ '

export RUSTUP_HOME=/usr/local/rustup
export CARGO_HOME=/usr/local/cargo
export PATH=/usr/local/cargo/bin:$PATH

run() {
    /run.sh
}