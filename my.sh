#!/usr/bin/env zsh
# remove fancy prompt when the $TERM is "dumb"
# this command should place in the first line of ~/.zshrc directly
# [[ $TERM == "dumb" ]] && unsetopt zle && PS1='$ ' && return 1

# alias for common commands
alias mv='mv -i'
default_proxy_host="127.0.0.1"
default_proxy_port="2333"
# alias proxy="export https_proxy=http://$proxy_host:$proxy_port http_proxy=http://$proxy_host:$proxy_port all_proxy=socks5://$proxy_host:$proxy_port"
function proxy() {
    # -p is for port, -h is for host
    while getopts "p:h:" opt; do
        case $opt in
            p) proxy_port="$OPTARG" ;;
            h) proxy_host="$OPTARG" ;;
            *) echo "Usage: proxy [-p port] [-h host]" >&2; return 1 ;;
        esac
    done
    export http_proxy="http://$proxy_host:$proxy_port"
    export https_proxy="http://$proxy_host:$proxy_port"
    export all_proxy="socks5://$proxy_host:$proxy_port"
    export HTTP_PROXY="http://$proxy_host:$proxy_port"
    export HTTPS_PROXY="http://$proxy_host:$proxy_port"
    export ALL_PROXY="socks5://$proxy_host:$proxy_port"
}
alias unproxy="unset https_proxy http_proxy all_proxy HTTP_PROXY HTTPS_PROXY ALL_PROXY"
alias cp='cp -i'

# alias for modern unix commands
## eza
if command -v eza &> /dev/null; then
    alias ls-ls='/bin/ls'
    alias ls='eza --all --long --group --group-directories-first --icons --header --time-style long-iso'
fi
## bat
BAT_BIN=""
if command -v bat &> /dev/null; then
    BAT_BIN="bat"
elif command -v batcat &> /dev/null; then
    BAT_BIN="batcat"
fi
if [[ -n "$BAT_BIN" ]]; then
    alias cat-cat='/bin/cat'
    alias cat="$BAT_BIN"
fi
## fd (for ubuntu package binary)
if ! command -v fd &> /dev/null && command -v fdfind &> /dev/null; then
    alias fd="fdfind"
fi

# alias for copilot
if command -v gh &> /dev/null; then
    alias copilot='gh copilot suggest -t shell -- '
fi

# env
export LANG=en_US.UTF-8
export LC_ALL=en_US.UTF-8
export TERM=xterm-256color
# editor
export EDITOR="emacs -nw"

# sshfs
# Detect the operating system
function detect_os() {
    if [[ "$OSTYPE" == "darwin"* ]]; then
        echo "macOS"
    elif [[ "$OSTYPE" == "linux-gnu"* ]]; then
        echo "Linux"
    else
        echo "Unknown OS"
    fi
}

# Function to check if 'user_allow_other' is enabled in /etc/fuse.conf
function check_allow_other_enabled() {
    if grep -q "^user_allow_other" /etc/fuse.conf; then
        echo "user_allow_other is already enabled."
        return 0
    else
        echo "user_allow_other is NOT enabled."
        return 1
    fi
}

# Function to show the dry-run command and request sudo permission to fix it
function enable_allow_other() {
    echo "Dry-run: The following command will be run to enable 'user_allow_other' in /etc/fuse.conf:"
    echo "    sudo bash -c \"echo 'user_allow_other' >> /etc/fuse.conf\""

    echo -n "Do you want to proceed and enable 'user_allow_other'? (y/n): "
    read answer
    if [[ "$answer" == "y" || "$answer" == "Y" ]]; then
        # Request sudo permission and append 'user_allow_other' to /etc/fuse.conf
        sudo bash -c "echo 'user_allow_other' >> /etc/fuse.conf"

        if [[ $? -eq 0 ]]; then
            echo "'user_allow_other' has been successfully enabled in /etc/fuse.conf."
            return 0
        else
            echo "Failed to enable 'user_allow_other'. Please check your permissions or manually edit /etc/fuse.conf."
        fi
    else
        echo "Operation cancelled. 'user_allow_other' remains disabled."
    fi
    return 1
}

# Define OS-specific sshfs mount commands
function mysshfs() {
    local os=$(detect_os)
    if [[ "$os" == "macOS" ]]; then
        sshfs -C -o kill_on_unmount,reconnect,allow_other,defer_permissions,auto_cache,nolocalcaches,no_readahead "$1" "$2"
    elif [[ "$os" == "Linux" ]]; then
        mount_command="sshfs -C -o dir_cache=no,transform_symlinks,idmap=user,reconnect,allow_other \"$1\" \"$2\""
        eval $mount_command 2> /dev/null
        if [[ $? -ne 0 ]]; then
            local error_message=$(eval $mount_command 2>&1)
            echo "$error_message" > /tmp/test.txt
            if echo "$error_message" | grep "user_allow_other" | grep -q "/etc/fuse.conf"; then
                if ! check_allow_other_enabled; then
                    enable_allow_other
                    if [[ $? -eq 0 ]]; then
                        # retry the mount command
                        echo "Retrying the mount command..."
                        eval $mount_command
                    fi
                else
                    echo "$error_message"
                    return 2
                fi
            fi
        fi
    else
        echo "Unsupported OS"
        return 1
    fi
}

# Define OS-specific unmount commands
function mysshfs-umount() {
    local os=$(detect_os)
    if [[ "$os" == "macOS" ]]; then
        diskutil unmount force "$1"
    elif [[ "$os" == "Linux" ]]; then
        umount "$1"
    else
        echo "Unsupported OS"
        return 1
    fi
}

# Unmount all sshfs mounts
function mysshfs-umount-all() {
    local os=$(detect_os)
    if [[ "$os" == "macOS" ]]; then
        ps ux | grep sshfs | grep -v grep | awk '{print $NF}' | xargs -n1 diskutil unmount force
    elif [[ "$os" == "Linux" ]]; then
        ps ux | grep sshfs | grep -v grep | awk '{print $NF}' | xargs -n1 umount
    else
        echo "Unsupported OS"
        return 1
    fi
}

# Reconnect function
function mysshfs-reconnect() {
    mysshfs-umount "$2"
    mysshfs "$1" "$2"
}

# all-in-one function
function mtsshfs() {
    # $1: remote directory
    # $2: local directory
    # on macOS:
    # try to mount the remote directory to the local directory
    # if failed, try to unmount the local directory and mount again
    # mysshfs "$1" "$2" || mysshfs-reconnect "$1" "$2"
    # BUT on Linux:
    # it is likely that the twice mount will succeed
    # Judge the mount status now via `ps ux | grep sshfs | grep -q "$2"`
    # if not mounted, mount it
    # if mounted, remount it
    ps ux | grep sshfs | grep -q "$2" && mysshfs-reconnect "$1" "$2" || mysshfs "$1" "$2"
}

# custom tmux command
function mtmux() {
    # If no parameter is passed, show interactive session selector
    # Otherwise, attach to or create a session with the name passed as the first parameter
    target_session="$1"
    if [ -z "$1" ]; then
        # Show interactive session selector using fzf
        selected_session=$(tmux list-sessions -F "#{session_name}" 2>/dev/null | fzf --height 40% --reverse --prompt="Select tmux session: ")
        if [ -z "$selected_session" ]; then
            # No session selected or fzf not available
            # tmux attach-session -t $default_session 2>/dev/null || tmux new-session -s $default_session
	    echo "No session selected. Exiting."
	    return 0
        else
	    target_session="$selected_session"
        fi
    elif [[ "$1" == "list" ]]; then
        tmux list-sessions
	return 0
    elif [[ "$1" == "help" ]]; then
        echo "Usage: mtmux [session_name]"
        echo "       mtmux list"
        echo "       mtmux help"
	return 0
    fi
    # now, the target_session variable is set
    # check if in a tmux session
    if [[ -n "$TMUX" ]]; then
	# If already in a tmux session, switch to the target session
	mtmux_in_session "$target_session"
    else
	# If not in a tmux session, attach to the target session or create it if it doesn't exist
	mtmux_out_session "$target_session"
    fi
}
function mtmux_in_session() {
    # switch to the target session if it exists, otherwise exit
    tmux switch-client -t "$1" 2>/dev/null || echo "Session '$1' does not exist."
}
function mtmux_out_session() {
    # attach to the target session if it exists, otherwise create a new session
    tmux attach-session -t "$1" 2>/dev/null || tmux new-session -s "$1"
}


# alias for container
if command -v podman &> /dev/null; then
    alias tmpctr="podman run -it --rm --log-driver none" # thx to taoky
fi

# ripgrep
if command -v rg &> /dev/null; then
    # https://github.com/BurntSushi/ripgrep/issues/1352#issuecomment-1959071755
    rgnc(){ Q="$1"; shift; rg --pretty --colors match:none -o ".{0,50}$Q.{0,50}" "$@" | rg --passthru "$Q" ;}
fi

# 0x0.st
0x0() { curl -F"file=@${1:--}" https://0x0.st | tee -a "$HOME/tmp/0x0.log"; }
