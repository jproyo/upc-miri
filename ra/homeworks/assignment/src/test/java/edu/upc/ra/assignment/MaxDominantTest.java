package edu.upc.ra.assignment;

import org.junit.Test;

import java.util.*;

import static org.junit.Assert.*;

public class MaxDominantTest {

    private MaxDominant target = new MaxDominant(1,1,1);

    @Test
    public void isDominant() {
        assertTrue(target.isDominant("1011", "1010"));
        assertFalse(target.isDominant("1011", "1101"));
        assertFalse(target.isDominant("1101", "1011"));
        assertFalse(target.isDominant("1001", "0010"));
        assertFalse(target.isDominant("0010", "1001"));
    }

    @Test
    public void compareAndSetDomiant(){
        Set<String> dominants = new HashSet<>();
        dominants.add("1011");
        dominants.add("1101");
        dominants.add("1010");
        target.compareAndSetDomiant(dominants, "1111");
        assertEquals(1, dominants.size());


        Set<String> dominants2 = new HashSet<>();
        dominants2.add("1010");
        dominants2.add("1101");
        target.compareAndSetDomiant(dominants2, "1011");
        assertEquals(2, dominants2.size());
        assertEquals(new HashSet<>(Arrays.asList("1101","1011")), dominants2);
    }
}