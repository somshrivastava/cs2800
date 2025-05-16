package pbt;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import java.util.*;

import org.junit.runner.RunWith;
import com.pholser.junit.quickcheck.Property;
import com.pholser.junit.quickcheck.runner.JUnitQuickcheck;

@RunWith(JUnitQuickcheck.class)
public class CollectionPropertyTests {
    @Property
    public void testSortPreservesSize(List<Integer> list) {
        List<Integer> copy = new ArrayList<>(list);
        Collections.sort(copy);
        assertEquals(list.size(), copy.size());
    }

    @Property
    public void testAddIfNotPresentDoesNotShrink(List<Integer> list, int elem) {
        int originalSize = list.size();
        if (!list.contains(elem)) list.add(elem);
        assertTrue(list.size() >= originalSize);
    }
}
