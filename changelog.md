0.3.9.0 2022-05-19
==================
- add tests for different types using sysN

0.3.8.0 2022-05-16
==================
- add ByteString to OutputDefault types

0.3.7.0 2022-05-13
==================
- add readlink, resolvelink

0.3.6.0 2022-04-10
==================
- add support for env modifications in system{,x}{,'} via ToCmdSpec
  (AbsFile,[Text],[EnvModFrag])

0.3.5.0 2022-03-07
==================
- add ordered type variables to MonadIO.File.*

0.3.4.0 2021-12-28
==================
- add MockIO.File.rename

0.3.3.0 2021-10-18
==================
- use ToCmdSpec for systemx{,'}, doProc{,'}

0.3.2.0 2021-10-18
==================
- change order of arguments to doProc{,'}

0.3.1.0 2021-10-18
==================

- add readFileY, flock, flockNB, unflock, systemx, systemx', doProc, doProc',
  mlMkCmd, mlMkCmd'
- export system'

0.3.0.2 2021-10-13
==================
- upgrade dependencies, including monadio-plus 2.0.0.0

0.3.0.1 2021-08-04
==================
- Fix up type ordering in T.Process for monaderror upgrade

0.3.0.0 2021-07-24
==================
- Add MockIO.Process

0.2.0.0 2021-06-03
==================
- Massive cleanup of openfile functions based on similar in MonadIO

0.1.4.0 2021-05-26
==================
- add MockIO.Directory

0.1.3.1 2021-05-10
==================
- add HasCallStack to all functions with MonadError in the type

0.1.3.0 2021-03-13
==================
-- add lfexists{,'}

0.1.2.1 2021-03-11
==================
- use monadio-plus 1.4.0.0, in which {l,}stat return FStat

0.1.2.0 2021-03-08
==================
- add fexists, fexists'

0.1.1.0 2021-03-07
==================
- major expansion & rewrite, part 2

0.1.0.0 2021-02-25
==================
- major expansion & rewrite, part 1

0.0.1.0 2021-02-05
==================
- introduce MockIO.File.{read,with{,Write},write}File
