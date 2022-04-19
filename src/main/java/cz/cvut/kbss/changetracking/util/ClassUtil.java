package cz.cvut.kbss.changetracking.util;

import cz.cvut.kbss.changetracking.exception.UnsupportedAttributeTypeException;

import java.lang.reflect.Array;
import java.util.*;

/**
 * Adapted from <a href="https://stackoverflow.com/a/18606772">source: SO</a>.
 */
public class ClassUtil {
	private static Set<Class<?>> getSuperclasses(Class<?> clazz) {
		final Set<Class<?>> result = new LinkedHashSet<>();
		final Queue<Class<?>> queue = new ArrayDeque<>();
		queue.add(clazz);
		/*if (clazz.isInterface()) {
			queue.add(Object.class); // optional
		}*/
		while (!queue.isEmpty()) {
			Class<?> c = queue.remove();
			if (result.add(c)) {
				Class<?> sup = c.getSuperclass();
				if (sup != null) queue.add(sup);
				//queue.addAll(Arrays.asList(c.getInterfaces()));
			}
		}
		return result;
	}

	private static Set<Class<?>> commonSuperclasses(Class<?> class1, Class<?> class2) {
		// begin with set from first hierarchy
		Set<Class<?>> result = getSuperclasses(class1);
		// remove non-superclasses of remaining
		result.removeIf(sup -> !sup.isAssignableFrom(class2));
		return result;
	}


	public static Optional<Class<?>> getCommonSuperclass(Class<?> class1, Class<?> class2) {
		var x = lowestClasses(commonSuperclasses(class1, class2));
		if (x.size() != 1) return Optional.empty(); // something is wrong

		var superclass = x.get(0);
		if (Object.class.equals(superclass)) return Optional.empty();

		return Optional.of(superclass);
	}

	private static List<Class<?>> lowestClasses(Collection<Class<?>> classes) {
		final LinkedList<Class<?>> source = new LinkedList<>(classes);
		final ArrayList<Class<?>> result = new ArrayList<>(classes.size());
		while (!source.isEmpty()) {
			Iterator<Class<?>> srcIt = source.iterator();
			Class<?> c = srcIt.next();
			srcIt.remove();
			while (srcIt.hasNext()) {
				Class<?> c2 = srcIt.next();
				if (c2.isAssignableFrom(c)) {
					srcIt.remove();
				} else if (c.isAssignableFrom(c2)) {
					c = c2;
					srcIt.remove();
				}
			}
			result.add(c);
		}
		result.trimToSize();
		return result;
	}

	/**
	 * For example: get an {@link Optional} of the {@code String[]} class for {@code "java.lang.String[]"} or an
	 * empty Optional for {@code "java.lang.String"}. This uses a naive method: simply checks if the last two
	 * characters of the class are "[]".
	 *
	 * @throws UnsupportedAttributeTypeException When the target class fails can't be found.
	 * @implNote The double cast is necessary to ensure the correct return type.
	 */
	@SuppressWarnings("unchecked")
	public static <T> Optional<Class<T[]>> getArrayClassByName(String className) {
		if ("[]".equals(className.substring(className.length() - 2))) {
			try {
				return Optional.of((Class<T[]>) ((Class<?>) getArrayClassFromSingular(Class.forName(className.substring(
					0,
					className.length() - 2
				)))));
			} catch (ClassNotFoundException e) {
				throw new UnsupportedAttributeTypeException(className);
			}
		}
		return Optional.empty();
	}

	@SuppressWarnings("unchecked")
	public static <T> Class<T[]> getArrayClassFromSingular(Class<T> target) {
		return (Class<T[]>) Array.newInstance(target, 0).getClass();
	}
}
