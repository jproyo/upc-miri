package edu.upc.ra.assignment;

import java.util.*;

public class QuickSelect {

    public static <T extends Number> int median(List<T> coll, Comparator<T> comp) {
        int result;
        int n = coll.size()/2;

        if (coll.size() % 2 == 0)  // even number of items; find the middle two and average them
            result = (nth(coll, n-1, comp).intValue() + nth(coll, n, comp).intValue()) / 2;
        else                      // odd number of items; return the one in the middle
            result = nth(coll, n, comp).intValue();

        return result;
    }


    public static <T> T nth(List<T> coll, int n, Comparator<T> comp) {
        T result, pivot;
        List<T> underPivot = new ArrayList<>(), overPivot = new ArrayList<>(), equalPivot = new ArrayList<>();

        // choosing a pivot is a whole topic in itself.
        // this implementation uses the simple strategy of grabbing something from the middle of the ArrayList.

        pivot = coll.get(n/2);

        // split coll into 3 lists based on comparison with the pivot

        for (T obj : coll) {
            int order = comp.compare(obj, pivot);

            if (order < 0)        // obj < pivot
                underPivot.add(obj);
            else if (order > 0)   // obj > pivot
                overPivot.add(obj);
            else                  // obj = pivot
                equalPivot.add(obj);
        } // for each obj in coll

        // recurse on the appropriate list

        if (n < underPivot.size())
            result = nth(underPivot, n, comp);
        else if (n < underPivot.size() + equalPivot.size()) // equal to pivot; just return it
            result = pivot;
        else  // everything in underPivot and equalPivot is too small.  Adjust n accordingly in the recursion.
            result = nth(overPivot, n - underPivot.size() - equalPivot.size(), comp);

        return result;
    }


}
