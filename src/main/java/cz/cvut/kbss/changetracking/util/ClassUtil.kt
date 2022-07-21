package cz.cvut.kbss.changetracking.util

import cz.cvut.kbss.changetracking.exception.UnsupportedAttributeTypeException
import java.lang.reflect.Array.newInstance
import java.util.*

/**
 * Adapted from [source: SO](https://stackoverflow.com/a/18606772).
 */
object ClassUtil {
	private fun getSuperclasses(clazz: Class<*>): MutableSet<Class<*>> {
		val result: MutableSet<Class<*>> = LinkedHashSet()
		val queue: Queue<Class<*>> = ArrayDeque()
		queue.add(clazz)
		while (!queue.isEmpty()) {
			val c = queue.remove()
			if (result.add(c)) {
				val sup = c.superclass
				if (sup != null) queue.add(sup)
			}
		}
		return result
	}

	private fun commonSuperclasses(class1: Class<*>, class2: Class<*>): Set<Class<*>> {
		// begin with set from first hierarchy
		val result = getSuperclasses(class1)
		// remove non-superclasses of remaining
		result.removeIf { sup: Class<*> -> !sup.isAssignableFrom(class2) }
		return result
	}

	@JvmStatic
	fun getCommonSuperclass(class1: Class<*>, class2: Class<*>): Optional<Class<*>> {
		val x = lowestClasses(commonSuperclasses(class1, class2))
		if (x.size != 1) return Optional.empty() // something is wrong
		val superclass = x[0]
		return if (Any::class.java == superclass) Optional.empty() else Optional.of(superclass)
	}

	@JvmStatic
	private fun lowestClasses(classes: Collection<Class<*>>): List<Class<*>> {
		val source = LinkedList(classes)
		val result = ArrayList<Class<*>>(classes.size)
		while (!source.isEmpty()) {
			val srcIt = source.iterator()
			var c = srcIt.next()
			srcIt.remove()
			while (srcIt.hasNext()) {
				val c2 = srcIt.next()
				if (c2.isAssignableFrom(c)) {
					srcIt.remove()
				} else if (c.isAssignableFrom(c2)) {
					c = c2
					srcIt.remove()
				}
			}
			result.add(c)
		}
		result.trimToSize()
		return result
	}

	/**
	 * For example: get an [Optional] of the `String[]` class for `"java.lang.String[]"` or an
	 * empty Optional for `"java.lang.String"`. This uses a naive method: simply checks if the last two
	 * characters of the class are "[]".
	 *
	 * @throws UnsupportedAttributeTypeException When the target class fails can't be found.
	 * @implNote The double cast is necessary to ensure the correct return type.
	 */
	@JvmStatic
	@Suppress("UNCHECKED_CAST")
	fun <T> getArrayClassByName(className: String): Optional<Class<Array<T>>> {
		return if ("[]" == className.substring(className.length - 2)) {
			try {
				Optional.of(
					getArrayClassFromSingular(
						Class.forName(
							className.substring(
								0,
								className.length - 2
							)
						)
					) as Class<Array<T>>
				)
			} catch (e: ClassNotFoundException) {
				throw UnsupportedAttributeTypeException(className)
			}
		} else Optional.empty()
	}

	@JvmStatic
	@Suppress("UNCHECKED_CAST")
	fun <T> getArrayClassFromSingular(target: Class<T>?): Class<Array<T>> {
		return newInstance(target, 0).javaClass as Class<Array<T>>
	}
}
