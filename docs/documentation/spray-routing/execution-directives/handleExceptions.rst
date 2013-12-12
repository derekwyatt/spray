.. _-handleExceptions-:

handleExceptions
================

Catches exceptions thrown by the inner route and handles them using the specified ``ExceptionHandler``.

Signature
---------

.. includecode:: /../spray-routing/src/main/scala/spray/routing/directives/ExecutionDirectives.scala
   :snippet: handleExceptions

Description
-----------

Using this directive is an alternative to using a global implicily defined ``ExceptionHandler`` that
applies to the complete route.

See :ref:`Exception Handling` for general information about options for handling exceptions.

Example
-------

.. includecode:: ../code/docs/directives/ExecutionDirectivesExamplesSpec.scala
   :snippet: handleExceptions
