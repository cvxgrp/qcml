#!/bin/sh

# clone ecos to this directory
if ! $(python -c 'import ecos' &> /dev/null); then 
    echo "ECOS module is not installed. Using git to clone...."; 
    CMD='git clone http://github.com/ifa-ethz/ecos.git'
    echo $CMD
    if ! $CMD; then
        echo "Error cloning; perhaps you forgot to install git?"
        exit 2
    fi

    cd ecos/python
    INSTALL_ECOS='python setup.py install'
    if ! -z "$1"; then
        INSTALL_ECOS="$INSTALL_ECOS --prefix $1"
    fi
    echo $INSTALL_ECOS
    if ! $INSTALL_ECOS; then
        echo "Error installing ECOS module; perhaps you need administrative privileges."
        echo "Alternatively, provide the prefix PATH for your local Python installation."
        echo "    sh get_ecos.sh LOCAL_PATH"
        cd ../..
        exit 2
    fi
    cd ../..
else
    echo "Already installed!"
fi
