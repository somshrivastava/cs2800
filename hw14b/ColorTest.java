package pbt;

import static org.junit.Assert.assertEquals;

import java.util.*;

import org.junit.runner.RunWith;
import com.pholser.junit.quickcheck.Property;
import com.pholser.junit.quickcheck.runner.JUnitQuickcheck;

@RunWith(JUnitQuickcheck.class)
public class ColorTest {
    public static class RGBColor {
        int r, g, b;

        public RGBColor(int r, int g, int b) {
            this.r = r;
            this.g = g;
            this.b = b;
        }

        public List<Integer> pack() {
            return Arrays.asList(1, r, g, b);
        }

        public static RGBColor unpack(List<Integer> list) {
            return new RGBColor(list.get(1), list.get(2), list.get(3));
        }

        @Override
        public boolean equals(Object o) {
            if (!(o instanceof RGBColor)) return false;
            RGBColor c = (RGBColor) o;
            return r == c.r && g == c.g && b == c.b;
        }

        @Override
        public int hashCode() {
            return Objects.hash(r, g, b);
        }
    }

    @Property
    public void testPackUnpackIsIdentity(int r, int g, int b) {
        RGBColor color = new RGBColor(r % 256, g % 256, b % 256);
        List<Integer> packed = color.pack();
        RGBColor result = RGBColor.unpack(packed);
        assertEquals(color, result);
    }
}
