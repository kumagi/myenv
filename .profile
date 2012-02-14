# ~/.profile: executed by the command interpreter for login shells.
# This file is not read by bash(1), if ~/.bash_profile or ~/.bash_login
# exists.
# see /usr/share/doc/bash/examples/startup-files for examples.
# the files are located in the bash-doc package.

# the default umask is set in /etc/profile; for setting the umask
# for ssh logins, install and configure the libpam-umask package.
#umask 022

# if running bash
if [ -n "$BASH_VERSION" ]; then
    # include .bashrc if it exists
    if [ -f "$HOME/.bashrc" ]; then
	. "$HOME/.bashrc"
    fi
fi

# set PATH so it includes user's private bin if it exists
if [ -d "$HOME/bin" ] ; then
    PATH="$HOME/bin:$PATH"
fi
# set couchbase
PATH="$PATH:/opt/couchbase/bin"

export BIBIMPUTS=~/Dropbox/bib/

export EC2_CERT=~/.ssh/cert-SG5UJ6PUAA3JNW3FEEM3GCVIKRODBCMI.pem
export EC2_PRIVATE_KEY=~/.ssh/pk-SG5UJ6PUAA3JNW3FEEM3GCVIKRODBCMI.pem

export EC2_URL=https://ec2.ap-northeast-1.amazonaws.com
