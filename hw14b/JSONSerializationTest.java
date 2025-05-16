package pbt;

import static org.junit.Assert.assertEquals;

import java.util.*;

import com.fasterxml.jackson.databind.ObjectMapper;

import org.junit.Test;

public class JSONSerializationTest {
    public static class Student {
        public String name;
        public List<String> interests;
        public String address;

        public Student() {}

        public Student(String name, List<String> interests, String address) {
            this.name = name;
            this.interests = interests;
            this.address = address;
        }

        @Override
        public boolean equals(Object o) {
            if (!(o instanceof Student)) return false;
            Student s = (Student) o;
            return Objects.equals(name, s.name) &&
                   Objects.equals(interests, s.interests) &&
                   Objects.equals(address, s.address);
        }

        @Override
        public int hashCode() {
            return Objects.hash(name, interests, address);
        }
    }

    @Test
    public void testJsonRoundTrip() throws Exception {
        ObjectMapper mapper = new ObjectMapper();
        Student s = new Student("John", Arrays.asList("music", "math"), "Boston");
        String json = mapper.writeValueAsString(s);
        Student result = mapper.readValue(json, Student.class);
        assertEquals(s, result);
    }
}
