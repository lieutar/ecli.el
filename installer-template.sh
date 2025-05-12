#! /usr/bin/env bash

###
### Setup Installer's context
###

here=`dirname $BASH_SOURCE`
install_el=$(mktemp)

cleanup(){
    [ -f $install_el ] && rm $install_el
}

trap cleanup int
trap cleanup EXIT

###
### Location to be installed
###

# Path to be installed
prefix=<<default-prefix>>

# Path to the definition to local settings
local_config=
# About local settings
# See README.md on https://github.com/lieutar/ecli.el/
# The section "Installer" will express it.

###
### Process arguments and interaction
###

while [ $# -gt 0 ]; do
    arg=$1
    shift
    case "$arg" in
         # Path to be installed ( Default <<default-prefix>> )
        --prefix|-P)
            prefix=$1
            case "$prefix"; in
                ""|-*)
                    echo "--prefix / -P requires an argument"
                    while [ -z "$prefix" ]; do
                          printf "Where? "
                          read prefix
                    done
                    ;;
                *)
                    shift
                    ;;
            esac
            ;;

        # Setting to local elpa mirror and pre-downloaded scripts
        --local-config|-C)
            local_config=$1
            case "$local_config" in
                "" |-*)
                    echo "--local-config / -C requires an argument"
                    while [ -z "$local_config" ]; do
                        printf "Where? "
                        read local_config
                    done
                    while [ -n "$local_config" ] &&\
                                ! [ -r "$local_config" ]; do
                        echo "Can't read $local_config"
                        printf "Where? "
                        echo "(ignore local-config specification if blank)"
                        printf "> "
                        read local_config
                    ;;
                *)
                    shift
                    ;;
            esac
            ;;

        # Process illegal arguments
        *)
            echo "Warning: unrecognized argument: $arg"
            printf "Continue? (yes/no , default=no):"
            read answer
            case "$answer" in
                yes|Yes|YES)
                    echo "Continue ..."
                    ;;
                *)
                    echo "Abort"
                    exit 1
                    ;;
            esac
        ;;
    esac
done

###
### Make Emacs Lisp installer
###

cat <<EOF >$install_el
<<logic-to-install>>
EOF

###
### Run the Emacs Lisp installer
###

# pass configurations by environment
export PREFIX=$prefix
export LOCAL_CONFIG_ELD=$local_config

if ! emacs --batch -Q -l $install_el -f eclinstall::main ; then
    echo "Installation failed." >&2
    exit 1
fi
