package edu.upc.ra.assignment;


import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.OpenOption;
import java.nio.file.Paths;
import java.util.*;
import java.util.stream.Collectors;

/**
 *
 * @author mac
 */
public class MaxDominant {

    private final Random random = new Random();

    // This is |S_n| suchs that S \subseteq U
    private final Integer samples;
    // This is m = log_2|U|
    private final Integer chainLength;
    //Number of experiments
    private final Integer experiments;

    private int numberAllOnesString = 0;

    private int expectedValue = 0;


    public MaxDominant(Integer samples, Integer chainLength, Integer experiments) {
        this.samples = samples;
        this.chainLength = chainLength;
        this.experiments = experiments;
    }


    /**
     * @param args the command line arguments
     */
    public static void main(String[] args) {

        Integer samples = Integer.valueOf(args[0]);
        Integer chainLength = Integer.valueOf(args[1]);
        Integer experiments = Integer.valueOf(args[2]);

        MaxDominant dominant = new MaxDominant(samples, chainLength, experiments);

        dominant.runExperiments();

        System.out.println("Amount Chains with All Ones: "+dominant.getNumberAllOnesString());

        System.out.println("Expected Value: "+dominant.getExpectedValue());

    }

    /**
     * Run all experiments and calculate expected value and ocurrence of all Ones
     */
    public void runExperiments(){
        Integer[] numberUndominatedStrings = new Integer[experiments];

        for(int i=0;i<experiments;i++){

            //Set S_n
            List<String> sn = generateSample();

            // Caluclate MAX(S_n)
            Set<String> maxUndominantSet = maxUndominant(sn);

            /**
             * This has been asked by the Teacher, so we need to store all MAX(S_n)
             */
            try {
                String collect = maxUndominantSet.stream().collect(Collectors.joining("\n"));
                Files.write(Paths.get("./sample_"+i+"_m_"+chainLength+"_sn"+samples+".txt"), collect.getBytes());
            } catch (IOException e) {
                e.printStackTrace();
            }

            // Count all ones chain ocurrence on Max(S_n)
            numberAllOnesString += Collections.frequency(maxUndominantSet, allOnes());

            //Store |MAX(S_n)| as a sample
            numberUndominatedStrings[i]= maxUndominantSet.size();

        }

        expectedValue = getMedian(numberUndominatedStrings);
    }


    /**
     * Generate Random Sample of {@code SUBSET_SAMPLE_LENGTH}
     * with each sample of length {@code M_LENGTH_BIT_CHAIN}
     * @return
     */
    public List<String>   generateSample(){
        List<String> sample = new ArrayList<>();
        for (int i = 0; i < samples; i++) {
            char[] buffer = new char[chainLength];
            for (int j = 0; j < chainLength; j++) {
                buffer[j] = tosCoin();
            }
            sample.add(new String(buffer));
        }
        return sample;
    }

    /**
     * Calculate mean with QuickSelect algorithm
     * @param numberUndominatedStrings
     * @return
     */
    public int getMedian(Integer[] numberUndominatedStrings){
        return QuickSelect.median(Arrays.asList(numberUndominatedStrings), Comparator.naturalOrder());
    }

    /**
     * Get Set of Max Non-Dominant ~ MAX(S_n)
     * @param sn
     * @return
     */
    public Set<String> maxUndominant(List<String> sn){
        // Base Case: Two or less elements to compare and check dominants between them
        if(sn.size() <= 2){
            Set<String> doms = new HashSet<>();
            compareAndSetDominantForEach(doms, new HashSet<>(sn));
            return doms;
        }


        // Establish medium index to recurse left and right
        int m = sn.size() / 2;
         // Get MAX(Sn) for Left
        Set<String> leftMaxUndominant = maxUndominant(sn.subList(0, m));
         // Get MAX(Sn) for Right
        Set<String> rightMaxUndominant = maxUndominant(sn.subList(m, sn.size()));

        Set<String> dominantsRec = new HashSet<>();
        // Merge left and right dominants subsets and keep non dominant between them
        compareAndSetDominantForEach(dominantsRec, leftMaxUndominant);
        compareAndSetDominantForEach(dominantsRec, rightMaxUndominant);
        return dominantsRec;
    }


    /**
     * This function makes the comparison of {@code elem}
     * against list of {@code dominants}
     *
     * - If {@code elem} dominates any other chain inside {@code dominants} list, remove that chain
     *   because it is not longer dominant chain
     * - After check every chain inside {@code dominants}, put element if the list is empty of if there is no
     * other chain that dominates {@code elem}
     *
     * @param dominants
     * @param elem
     */
    public void compareAndSetDomiant(Set<String> dominants, String elem) {
        Iterator<String> iterator = dominants.iterator();
        boolean addElem = true;
        while(iterator.hasNext()){
            String next = iterator.next();
            if(isDominant(next,elem)) {
                addElem = false;
                break;
            }
            if(isDominant(elem, next)){
                iterator.remove();
            }
        }
        if(dominants.isEmpty() || addElem){
            dominants.add(elem);
        }
    }

    /**
     * Returns true if a dominates b. False otherwise
     * @param a
     * @param b
     * @return
     */
    public boolean isDominant(String a, String b){
        boolean isDominant = true;
        for (int i = 0; i < a.length(); i++) {
            if(a.charAt(i) < b.charAt(i)) {
                isDominant = false;
                break;
            }
        }
        return isDominant;
    }


    private void compareAndSetDominantForEach(Set<String> dominants, Set<String> subSet){
        for (String elem: subSet) {
            compareAndSetDomiant(dominants, elem);
        }
    }


    /**
     * All ones element list
     * @return
     */
    private String allOnes(){
        char[] ones = new char[chainLength];
        for (int i = 0; i < chainLength ; i++) {
            ones[i] = '1';
        }
        return new String(ones);
    }




    /**
     * Tos a coin
     * @return
     */
    private char tosCoin(){
        return (char)(random.nextInt(2)+'0');
    }


    public int getExpectedValue() {
        return expectedValue;
    }


    public int getNumberAllOnesString() {
        return numberAllOnesString;
    }
}
