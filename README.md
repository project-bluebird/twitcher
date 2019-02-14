
# Twitcher
Front-end for [Bluebird](https://github.com/alan-turing-institute/bluebird) for monitoring the simulation.

Wikipedia [article on birdwatching](https://en.wikipedia.org/wiki/Birdwatching) defines *twitcher* as:
> The term twitcher, sometimes misapplied as a synonym for birder, is reserved for those who travel long distances to see a rare bird that would then be ticked, or counted on a list.

## Running Twitcher

Twitcher is built using .NET Core and Fable. 

### Requirements

* [dotnet SDK](https://www.microsoft.com/net/download/core) 2.1 or higher
* [node.js](https://nodejs.org) with [yarn](https://yarnpkg.com/lang/en/)
* For development: Visual Studio Code with [Ionide plugin](http://ionide.io/).

### Building and running the app

#### In development mode

*If you are using Windows replace `./fake.sh` by `fake.cmd`*

1. Run: `./fake.sh build -t Watch`
2. Go to [http://localhost:8080/](http://localhost:8080/)

*On Unix you may need to run `chmod a+x fake.sh`*

In development mode, we activate:

- [Hot Module Replacement](https://fable-elmish.github.io/hmr/), modify your code and see the change on the fly

#### Build for production

*If you are using Windows replace `./fake.sh` by `fake.cmd`*

1. Run: `./fake.sh build`
2. All the files needed for deployment are under the `output` folder.
