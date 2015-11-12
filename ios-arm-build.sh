#!/bin/bash
#
# This script build for arm iOS and then copies the
# static library to XCode
ARCH_OS=arm-apple-darwin10
ARCH=arm
if [ "$IOS_SCRIPTS" = "" -o "$XCODE_PROJECT_BUILD_DIR" = "" ]; then
    echo "Either IOS_SCRIPTS or XCODE_PROJECT_BUILD_DIR is not set"
    exit 1
fi


$ARCH_OS-cabal configure -fios $@ && \
    $ARCH_OS-cabal build exe:HXNetworkingStaticLib
[ $? -eq 0 ] || exit 1

echo
echo Copying to HXNetworking to $XCODE_PROJECT_BUILD_DIR
echo
if [ ! -d "$XCODE_PROJECT_BUILD_DIR/lib/$ARCH" ]; then
    mkdir -p $XCODE_PROJECT_BUILD_DIR/lib/$ARCH
fi
cp dist/$ARCH/build/HXNetworkingStaticLib/HXNetworkingStaticLib.a \
   "$XCODE_PROJECT_BUILD_DIR/lib/$ARCH/libHXNetworking-$ARCH.a"
