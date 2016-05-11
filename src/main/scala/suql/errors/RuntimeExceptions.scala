package suql.errors

trait SuqlThrowable
trait SuqlError extends SuqlThrowable
trait SuqlException extends SuqlThrowable

class SuqlRuntimeException(message: String) extends Exception with SuqlException
class SuqlRuntimeTypeException(message: String) extends Exception with SuqlException
class SuqlRuntimeArgumentException(message: String) extends Exception with SuqlException
class SuqlQueryException(message: String) extends Exception with SuqlException

class SuqlRuntimeError(message: String) extends Error with SuqlError
class SuqlNotImplementedError(message: String) extends Error with SuqlError