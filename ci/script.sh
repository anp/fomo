# This script takes care of testing your crate

set -ex

main() {
    # don't put effort in to run tests if they can't pass style check
    cargo fmt -- --write-mode diff

    # TODO figure out code coverage
    # TODO run benchmarks on nightly (just to ensure they stay building)

    cross build --target $TARGET
    cross build --target $TARGET --release

    if [ ! -z $DISABLE_TESTS ]; then
        return
    fi

    cross test --target $TARGET
    cross test --target $TARGET --release
}

# we don't run the "test phase" when doing deploys
if [ -z $TRAVIS_TAG ]; then
    main
fi
