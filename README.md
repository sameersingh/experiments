experiments
===========

## Requirements

## Installation

```xml
<dependency>
  <groupId>org.sameersingh.experiments</groupId>
  <artifactId>experiments</artifactId>
  <version>0.1-SNAPSHOT</version>
</dependency>
```

## Basic Usage

For the most part, this project acts like a simple, single-table DB.

### Creation

The first step is to specify the `Spec` for our experiments, i.e. what are the descriptors of your experiments.

```scala
val spec = new Spec
```
Given a spec, a single data `Point` consists of an optional value for each column.

```scala
val point = new Point(spec)
```

An experiment object contains multiple data points that share the same `spec`.

```scala
val exp = new Experiment
exp += point
```

### I/O

Experiments can be written to disk in two formats, tab-separated values or json.

```scala
exp.writeToFile(filename)
exp.writeToData(filename)
```

From the latter, we can read an experiment back into memory.

```scala
val exp = Experiment.fromFile(filename)
```

### Filtering, Truncating, etc.

## Plotting

## Aggregating

