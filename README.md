# ishallopentoday

Is hall open today?

## Set-up

Install [stack](https://haskellstack.org), clone this repo then from the main folder, run

    /usr/share/git/workdir/git-new-workdir . dist gh-pages

Install [closure compiler](https://developers.google.com/closure/compiler/).

## Deployment

The following command builds the relevant libraries and executables, and dumps the resulting website into the `dist` directory.

    ./build.sh -P

`-P` switches on the production mode options
