#!/bin/bash
#
# This script build for iOS on all 3 architectures and then copies each version of the
# static library to XCode

source settings.env

if [ "$IOS_SCRIPTS" = "" -o "$XCODE_PROJECT_BUILD_DIR" = "" ]; then
    echo "Either IOS_SCRIPTS or XCODE_PROJECT_BUILD_DIR is not set"
    exit 1
fi

for arch in i386 arm; do
    env IOS_SCRIPTS=$IOS_SCRIPTS XCODE_PROJECT_BUILD_DIR=$XCODE_PROJECT_BUILD_DIR ./ios-$arch-build.sh $@
#cabal-ios install && \
    #cabal-ios configure -fios $@ && \
    #cabal-ios build exe:HXNetworkingStaticLib
    [ $? -eq 0 ] || exit 1
done

echo
echo Merging libraries
echo

lipo dist/arm/build/HXNetworkingStaticLib/HXNetworkingStaticLib.a dist/i386/build/HXNetworkingStaticLib/HXNetworkingStaticLib.a -create -output HXNetworkingStaticLib.a

echo
echo Copying libraries
echo

for file in $(find ./dist/ -name *_stub.h); do
    mv "$file" $XCODE_PROJECT_BUILD_DIR/SDL/lib
done

mv HXNetworkingStaticLib.a $XCODE_PROJECT_BUILD_DIR/SDL/lib/

echo
echo Copying config file
echo

cp HaskelliOS.xcconfig $XCODE_PROJECT_BUILD_DIR/


echo
echo Cleaning up
echo
rm -rf liba.a dist/
