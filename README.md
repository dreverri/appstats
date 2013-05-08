Appstats collects raw event data and provides an API for reading and
summarizing that data. Inspired by [Cube](https://github.com/square/cube) but
written in Erlang using LevelDB.

## HTTP API

A very simple HTTP API has been provided to explore collected data.

Return a list of running sessions:

```
GET /sessions
```

Return the first and last known time points for a running session:

```
GET /session/:session_id/timespan
```

Return the names of events collected during a particular period of time:

```
GET /session/:session_id/names?start=1367995259000&stop=1367995260000
```

Return summary data for a particular event during a period of time at the
specified step interval:

```
GET /session/:session_id/data?name=riakc.get&start=1367995259000&stop=1367995260000&step=1000
```

## Example

A very minimal example of how one could use this application is provided. The
example is a single riakc client worker repeatedly getting a random key from
Riak. The example assumes there is a Riak instance listening for PB connections
on "127.0.0.1:8087".

To start the example:

```
cd riakc_example
make start
```

Data collected from the example can be viewed in a browser at
[http://localhost:8080](http://localhost:8080)

![screenshot](https://github.com/dreverri/appstats/raw/master/riakc_example/screenshot.png)

## TODO

* Better visualizations (priv/www/index.html)
* Look into [Vega](https://github.com/trifacta/vega)
* Support queries other than exact name match

The current visualization uses [Cubism](https://github.com/square/cubism) to
update the visuals in real time. The real time updates are neat but not
necessary. Better, non-real time, visualizations should be developed.
