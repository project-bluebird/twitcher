# Twitcher
Front-end for [Bluebird](https://github.com/alan-turing-institute/bluebird) for monitoring the simulation.

Wikipedia [article on birdwatching](https://en.wikipedia.org/wiki/Birdwatching) defines *twitcher* as:
> The term twitcher, sometimes misapplied as a synonym for birder, is reserved for those who travel long distances to see a rare bird that would then be ticked, or counted on a list.


## Running Twitcher

Twitcher is built using .NET Core and Fable. 

### Requirements

* [dotnet SDK](https://www.microsoft.com/net/download/core) 2.1 or higher
* [node.js](https://nodejs.org) with [npm](https://www.npmjs.com/)
* For development: Visual Studio Code with [Ionide](http://ionide.io/).

### Building and running the app

* Install JS dependencies: `npm install`
* Start Webpack dev server: `npm start`
* After the first compilation is finished, in your browser open: http://localhost:8080/

Any modification you do to the F# code will be reflected in the web page after saving.
