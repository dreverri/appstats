Appstats collects raw event data and provides an API for reading and
summarizing that data. Inspired by [Cube](https://github.com/square/cube) but
written in Erlang using LevelDB.

## HTTP API

A very simple HTTP API has been provided to explore collected data.

Return a list of running sessions:

```
GET /sessions
```

```json
["226635918053873426086712796756526707779"]
```

Return the first and last known time points for a running session:

```
GET /session/:session_id/timespan
```

```json
{
  "start": 1368115003903,
  "stop": 1368139163500
}
```

Return the names of events collected during a particular period of time:

```
GET /session/:session_id/names?start=1367995259000&stop=1367995260000
```

```json
[
  "riak.vnode_get.1164634117248063262943561351070788031288321245184",
  "riak.vnode_get.0",
  "riak.vnode_get.890602560248518965780370444936484965102833893376",
  "riak.vnode_get.1096126227998177188652763624537212264741949407232",
  "riak.vnode_get.1255977969581244695331291653115555720016817029120",
  "riak.vnode_get.1301649895747835411525156804137939564381064921088"
]
```

Return summary of data for all events during a period of time at a specified
step interval:

```
GET /session/:session_id/data?start=1368139003903&stop=1368139063904&step=60000
```

```json
[
  {
    "timestamp": 1368139003903,
    "data": {
      "riak.vnode_get.0": {
        "count": 115,
        "min": 36,
        "mean": 69.10434782608695,
        "max": 262
      },
      "riak.vnode_get.1164634117248063262943561351070788031288321245184": {
        "count": 144,
        "min": 33,
        "mean": 65.31944444444444,
        "max": 160
      },
      "riak.vnode_get.1324485858831130769622089379649131486563188867072": {
        "count": 140,
        "min": 35,
        "mean": 69.33571428571429,
        "max": 458
      }
    }
  },
  {
    "timestamp": 1368139063903,
    "data": {
      "riak.vnode_get.1164634117248063262943561351070788031288321245184": {
        "count": 148,
        "min": 31,
        "mean": 65.4391891891892,
        "max": 339
      },
      "riak.vnode_get.0": {
        "count": 150,
        "min": 26,
        "mean": 63.82,
        "max": 162
      },
      "riak.vnode_get.890602560248518965780370444936484965102833893376": {
        "count": 142,
        "min": 24,
        "mean": 66.90845070422536,
        "max": 233
      }
    }
  }
]
```

Return summary data for a particular event during a period of time at a
specified step interval:

```
GET /session/:session_id/data?name=riak.vnode_get.0&start=1368139003903&stop=1368139063904&step=60000
```

```json
[
  {
    "timestamp": 1368139003903,
    "data": {
      "riak.vnode_get.0": {
        "count": 115,
        "min": 36,
        "mean": 69.10434782608695,
        "max": 262
      }
    }
  },
  {
    "timestamp": 1368139063903,
    "data": {
      "riak.vnode_get.0": {
        "count": 150,
        "min": 26,
        "mean": 63.82,
        "max": 162
      }
    }
  }
]
```

## Examples

### StatsD Server

There is a basic statsd server implemented in the `statsd_server` directory.
This application will listen on port 8125 for StatsD messages and store them in
an Appstats session.

To start the server:

```
cd examples/statsd_server
make start
```

The collected data can be viewed in a browser at
[http://localhost:8080](http://localhost:8080)

### Riak Worker

This example is a single riakc client worker repeatedly getting a random key from
Riak. The example assumes there is a Riak instance listening for PB connections
on "127.0.0.1:8087".

To start the example:

```
cd examples/riakc_example
make start
```

Data collected from the example can be viewed in a browser at
[http://localhost:8080](http://localhost:8080)

![screenshot](https://github.com/dreverri/appstats/raw/master/examples/riakc_example/screenshot.png)

## TODO

### Better Visualizations

The current visualization uses [Cubism](https://github.com/square/cubism) to
update the visuals in real time. The real time updates are neat but not
necessary. Better, non-real time, visualizations should be developed (e.g.
a heatmap of a metrics histogram).

It may be useful to look into using [Vega](https://github.com/trifacta/vega)

### Queries

Support queries other than exact name match. Possible queries:

* "riak.vnode_get.\*" - group all vnode_get stats into a single metric per timestamp
* "riak.vnode_get.\*|riak.vnode_put.\*" - return 2 stats per timestamp, each stat would group vnode_gets and vnode_puts

### Retrieving names is slow

Retrieving names is slow.

### More data over HTTP

Appstats computes much more information than is currently exposed over HTTP.
This information should be exposed (e.g. percentiles, histograms). 
