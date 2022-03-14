package cz.cvut.kbss.changetracking.util;

import org.jetbrains.annotations.Nullable;

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


	// TODO: Optional<>
	@Nullable
	public static Class<?> getCommonSuperclass(Class<?> class1, Class<?> class2) {
		var x = lowestClasses(commonSuperclasses(class1, class2));
		if (x.size() != 1) return null; // something is wrong

		var superclass = x.get(0);
		if (Object.class.equals(superclass)) return null;

		return superclass;
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
}
