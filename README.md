# *Taranos Core* #

### What is Taranos Core? ###
This is the service core component of [_Taranos:CSF_](https://github.com/taranos/taranoscsf).  It may be used in conjunction with the [*Taranos Reference Server*](https://github.com/taranos/taranoscsf-refserver) as an API reference microservice or it can be embedded within another application to provide Taranos modeling services.

### Getting Started ###

Step 1:  The Taranos Project uses [_SBT_](http://www.scala-sbt.org/) for Scala build management.  It can be downloaded from [here](http://www.scala-sbt.org/download.html).

Step 2:  Build and test the service core as follows:

```
$ git clone https://github.com/taranos/taranoscsf-core.git
$ cd taranoscsf-core
$ sbt test
```

If tests are satisfactory then build the service core artifact (``taranos-core_*.jar``) by running:

```
$ sbt package
```

This will create the artifact jar as ``target/scala-2.11/taranos-core_*.jar``.

### Next Steps ###

- Explore project [code documentation](http://rawgit.com/taranos/taranoscsf-core/master/docs/api/index.html)

- Learn [_Taranos:CSF_ concepts](https://github.com/taranos/taranoscsf/wiki/Domain-Model-Concepts)

- Work with the [Pseudo-API tutorials](https://github.com/taranos/taranoscsf/wiki/PAPI-Tutorials)

- Read FAQs

- Go to the [Taranos Project](https://github.com/taranos/taranoscsf) main project 
 
