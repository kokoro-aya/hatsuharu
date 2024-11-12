# A simple code evaluator using scala.js & laminar

Project: TBD


# Several tricks to make the project compile

This project is built with several sub projects. You are visiting the last project, which depends on others (corelang, 
for example). See `build.sbt` for more info.

It's a bit tricky to make a proper environment and compile this project. In general, this visualizer requires the
following conditions:

- A JDK of version 11
  
  If not satisfied, `Scala: java.lang.NoClassDefFoundError: Could not initialize class sbt.internal.parser.SbtParser`
  and `bad constant pool index: 0 at pos: <a big number>` errors will be raised while you are trying to open the SBT
  portal, regardless in IDEA or your console.

- Ensure that in your IDEA settings, under `Build, Execution, Deployment > Build Tools > sbt` panel, in `sbt shell`
section, at least `project reload` is chosen. It's also recommended to enable `builds` option under this section. 

  Otherwise, the project reload on startup will hang forever.
  
- Check that all sub-projects have same sbt and scala versions

  You can check the sbt version under `./project/build.properties` for each subproject and `./build.sbt` for scala
  version.

  Since required by ScalablyTyped, we lock sbt version to 1.8.0 (1.8.x required). As for scala version, we have chosen
  currently 3.5.1.

  - If scala version changed, you may need to go to each sub project to rebuild a correct version then return to this 
  repository for rebuilding the whole project.

- A `npm install` is also required prior to `sbt compile` in this project.

  - Otherwise, the compilation will fail since it's up to us to handle the JS dependencies.

You should be all set if you have followed these indications.

Calling `npm run dev` should be sufficient to run the project in all, but you can also combine sbt and npm commands.

For testing, call `sbt test`.