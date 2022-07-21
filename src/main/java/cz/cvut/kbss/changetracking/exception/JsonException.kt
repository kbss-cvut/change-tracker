package cz.cvut.kbss.changetracking.exception

/**
 * [JsonException] is a [ChangeTrackingException] thrown when an attribute's value fails to de/serialize
 * from or to JSON.
 */
class JsonException(verbClause: String, cause: Throwable) :
	ChangeTrackingException("Failed to $verbClause JSON", cause)
