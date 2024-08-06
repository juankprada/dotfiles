# This file must be used with "source bin/activate.csh" *from csh*.
# You cannot run it directly.

# Unset irrelevant variables.
which kerl_deactivate >& /dev/null
if ( $status == 0 ) then
    kerl_deactivate
endif

set _KERL = /home/juankprada/.local/bin/kerl

if ( "$_KERL" != "" ) then
  set _KERL_VERSION = `$_KERL version`
endif

if ( $?_KERL_VERSION ) then
  if ( "$_KERL_VERSION" != "" && "$_KERL_VERSION" != "4.2.0" ) then
    echo "WARNING: this Erlang/OTP installation appears to be stale. Please consider reinstalling."
    echo "         It was created with kerl 4.2.0, and the current version is $_KERL_VERSION."
  endif
endif

unset _KERL_VERSION _KERL

alias add_cleanup 'set _KERL_CLEANUP = '\"'\!:1*; $_KERL_CLEANUP'\"''
alias _kerl_remove_el 'setenv \!:1 `echo $\!:1 | sed -r "s%\!:2*%%"`'
alias _kerl_cleanup_manpath 'eval "if ( '\''${MANPATH}'\'' == '\'''\'' ) then \\
    unsetenv MANPATH \\
endif"'

set _KERL_CLEANUP = ""

set _KERL_ACTIVE_DIR = "/home/juankprada/.local/bin/erlang/27.0.1"
add_cleanup unset _KERL_ACTIVE_DIR

if ( $?REBAR_CACHE_DIR ) then
    add_cleanup setenv REBAR_CACHE_DIR $REBAR_CACHE_DIR
else
    add_cleanup unsetenv REBAR_CACHE_DIR
endif
setenv REBAR_CACHE_DIR "/home/juankprada/.local/bin/erlang/27.0.1/.cache/rebar3"

if ( $?REBAR_PLT_DIR ) then
    add_cleanup setenv REBAR_PLT_DIR $REBAR_PLT_DIR
else
    add_cleanup unsetenv REBAR_PLT_DIR
endif
setenv REBAR_PLT_DIR "/home/juankprada/.local/bin/erlang/27.0.1"

set _KERL_PATH_REMOVABLE = "/home/juankprada/.local/bin/erlang/27.0.1/bin"
add_cleanup setenv PATH $PATH
setenv PATH "${_KERL_PATH_REMOVABLE}:$PATH"
unset _KERL_PATH_REMOVABLE

set _KERL_MANPATH_REMOVABLE = "/home/juankprada/.local/bin/erlang/27.0.1/lib/erlang/man:/home/juankprada/.local/bin/erlang/27.0.1/man"
if ( $?MANPATH ) then
    if ( "$MANPATH" == "" ) then
        setenv MANPATH "${_KERL_MANPATH_REMOVABLE}"
    else
        setenv MANPATH "${_KERL_MANPATH_REMOVABLE}:$MANPATH"
    endif
else
    add_cleanup _kerl_cleanup_manpath
    setenv MANPATH "${_KERL_MANPATH_REMOVABLE}"
endif
add_cleanup _kerl_remove_el MANPATH ${_KERL_MANPATH_REMOVABLE}
add_cleanup _kerl_remove_el MANPATH ${_KERL_MANPATH_REMOVABLE}:
unset _KERL_MANPATH_REMOVABLE

set _KERL_ERL_CALL_REMOVABLE = /home/juankprada/.local/bin/erlang/27.0.1/lib/erl_interface-5.5.2/bin
if ("$_KERL_ERL_CALL_REMOVABLE" != "") then
    add_cleanup setenv PATH $PATH
    setenv PATH "${_KERL_ERL_CALL_REMOVABLE}:$PATH"
endif
unset _KERL_ERL_CALL_REMOVABLE

if ( -f "/home/juankprada/.kerlrc.csh" ) then
    source "/home/juankprada/.kerlrc.csh"
endif

if ( $?KERL_ENABLE_PROMPT ) then
    if ( $?KERL_PROMPT_FORMAT ) then
        set FRMT = "$KERL_PROMPT_FORMAT"
    else
        set FRMT = "(%BUILDNAME%)"
    endif
    set PROMPT = `echo "$FRMT" | sed 's^%RELEASE%^27.0.1^;s^%BUILDNAME%^27.0.1^'`
    if ( $?prompt ) then
        add_cleanup set prompt = '$prompt'
        set prompt = "$PROMPT$prompt"
    else
        add_cleanup unset prompt
        set prompt = "$PROMPT"
    endif
    unset FRMT PROMPT
endif

rehash

unalias add_cleanup
eval 'alias kerl_deactivate "$_KERL_CLEANUP; unalias kerl_deactivate _kerl_remove_el _kerl_cleanup_manpath"'
unset _KERL_CLEANUP
