#!/usr/bin/env bash
# Source this file to define proxy-system, unproxy-system, proxy-system-status,
# and dns-system.
# Run proxy-system --help for usage. Sourcing the file does not change settings.
# Commands verified against `networksetup -help` and Apple's documentation:
# https://support.apple.com/guide/remote-desktop/about-networksetup-apdd0c5a2d5/mac

function _mt_macos_check_service() {
    local service="$1" services candidate
    if [[ "${OSTYPE:-}" != darwin* ]]; then
        echo 'System network settings require macOS.' >&2
        return 1
    fi
    services=$(networksetup -listallnetworkservices) || return 1
    # The first line explains the asterisk marking disabled services.
    services="${services#*$'\n'}"
    while IFS= read -r candidate; do
        if [[ "$candidate" == "$service" || "$candidate" == "*$service" ]]; then
            return 0
        fi
    done <<<"$services"
    printf 'Unknown network service: %s. Run networksetup -listallnetworkservices.\n' "$service" >&2
    return 1
}

function _mt_macos_networksetup() {
    local result=0
    if [[ $EUID -eq 0 ]]; then
        networksetup "$@" || result=$?
    else
        sudo networksetup "$@" || result=$?
    fi
    if [[ $result -ne 0 ]]; then
        echo 'macOS network update failed; system settings may be partially changed.' >&2
    fi
    return "$result"
}

function _mt_macos_proxy() {
    local service="$1" host="$2" port="$3"
    _mt_macos_check_service "$service" || return 1
    # Setters enable all three proxies. The final "off" disables authentication.
    _mt_macos_networksetup -setwebproxy "$service" "$host" "$port" off || return 1
    _mt_macos_networksetup -setsecurewebproxy "$service" "$host" "$port" off || return 1
    _mt_macos_networksetup -setsocksfirewallproxy "$service" "$host" "$port" off || return 1
    _mt_macos_networksetup -setautoproxystate "$service" off || return 1
    _mt_macos_networksetup -setproxyautodiscovery "$service" off
}

function _mt_macos_unproxy() {
    local service="$1" option result=0
    _mt_macos_check_service "$service" || return 1
    # Attempt every mode even if an earlier setting fails.
    for option in -setwebproxystate -setsecurewebproxystate \
        -setsocksfirewallproxystate -setautoproxystate -setproxyautodiscovery; do
        _mt_macos_networksetup "$option" "$service" off || result=1
    done
    return "$result"
}

function _mt_macos_proxy_status() {
    local service="${1:-${MT_MACOS_PROXY_SERVICE:-Wi-Fi}}" option result=0
    _mt_macos_check_service "$service" || return 1
    for option in -getwebproxy -getsecurewebproxy -getsocksfirewallproxy \
        -getautoproxyurl -getproxyautodiscovery; do
        printf '%s (%s):\n' "$service" "${option#-get}"
        networksetup "$option" "$service" || result=1
    done
    return "$result"
}

function _mt_macos_usage() {
    cat <<'EOF'
Usage:
  proxy-system [-p port] [-h host] [-n service]
  unproxy-system [-n service]
  proxy-system-status [-n service]
  dns-system [-n service] {127.0.0.1|default}
  proxy-system --help

Options:
  -p port     Proxy port (default: 2333).
  -h host     Proxy host (default: 127.0.0.1).
  -n service  Network service (default: Wi-Fi; override with
              MT_MACOS_PROXY_SERVICE, or MT_MACOS_DNS_SERVICE for DNS).

Examples:
  proxy-system
  proxy-system -p 7890 -n "USB Ethernet"
  proxy-system-status -n "USB Ethernet"
  unproxy-system -n "USB Ethernet"
  dns-system 127.0.0.1
  dns-system default
  dns-system -n "USB Ethernet" 127.0.0.1

Changes persist after this command exits; sudo may request a password.
Proxy mode enables HTTP, HTTPS, and SOCKS5 with the same host and port,
and disables PAC and automatic discovery.
Unproxy disables all proxy modes on the selected service without restoring
earlier settings. Shell proxy environment variables are managed separately.
DNS default clears manual DNS servers to use network-provided DNS settings.
EOF
}

function _mt_macos_proxy_command() {
    local action="$1" option_spec='n:'
    local host='127.0.0.1' port='2333'
    local service="${MT_MACOS_PROXY_SERVICE:-Wi-Fi}"
    local OPTIND=1 opt
    shift
    if [[ "${1:-}" == --help ]]; then
        _mt_macos_usage
        return 0
    fi
    case "$action" in
        proxy) option_spec='p:h:n:' ;;
        unproxy | status) ;;
        *)
            _mt_macos_usage >&2
            return 2
            ;;
    esac
    # -p is for port, -h is for host, -n selects the service.
    while getopts "$option_spec" opt; do
        case "$opt" in
            p) port="$OPTARG" ;;
            h) host="$OPTARG" ;;
            n) service="$OPTARG" ;;
            *)
                _mt_macos_usage >&2
                return 2
                ;;
        esac
    done
    shift $((OPTIND - 1))
    if [[ $# -ne 0 || -z "$service" ]]; then
        _mt_macos_usage >&2
        return 2
    fi
    if [[ "${OSTYPE:-}" != darwin* ]]; then
        echo 'System proxy settings require macOS.' >&2
        return 1
    fi
    case "$action" in
        proxy)
            case "$port" in
                '' | *[!0-9]*)
                    echo 'Proxy port must be an integer from 1 to 65535.' >&2
                    return 2
                    ;;
            esac
            if [[ ${#port} -gt 5 ]] || ((10#$port < 1 || 10#$port > 65535)); then
                echo 'Proxy port must be an integer from 1 to 65535.' >&2
                return 2
            fi
            port=$((10#$port))
            # networksetup takes bare IPv6 addresses, without URL brackets.
            host="${host#\[}"
            host="${host%\]}"
            if [[ -z "$host" ]]; then
                echo 'Proxy host must not be empty.' >&2
                return 2
            fi
            _mt_macos_proxy "$service" "$host" "$port"
            ;;
        unproxy) _mt_macos_unproxy "$service" ;;
        status) _mt_macos_proxy_status "$service" ;;
    esac
}

function proxy-system() {
    _mt_macos_proxy_command proxy "$@"
}

function unproxy-system() {
    _mt_macos_proxy_command unproxy "$@"
}

function proxy-system-status() {
    _mt_macos_proxy_command status "$@"
}

function dns-system() {
    local service="${MT_MACOS_DNS_SERVICE:-Wi-Fi}" server
    local OPTIND=1 opt
    if [[ "${1:-}" == --help ]]; then
        _mt_macos_usage
        return 0
    fi
    while getopts 'n:' opt; do
        case "$opt" in
            n) service="$OPTARG" ;;
            *)
                _mt_macos_usage >&2
                return 2
                ;;
        esac
    done
    shift $((OPTIND - 1))
    if [[ $# -ne 1 || -z "$service" ]]; then
        _mt_macos_usage >&2
        return 2
    fi
    case "$1" in
        127.0.0.1) server='127.0.0.1' ;;
        default) server='Empty' ;;
        *)
            _mt_macos_usage >&2
            return 2
            ;;
    esac
    _mt_macos_check_service "$service" || return 1
    _mt_macos_networksetup -setdnsservers "$service" "$server"
}
