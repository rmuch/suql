package suql.errors

class SuqlRuntimeException(message: String) extends Exception
class SuqlRuntimeTypeException(message: String) extends Exception
class SuqlQueryException(message: String) extends Exception
class SuqlRuntimeError(message: String) extends Error
