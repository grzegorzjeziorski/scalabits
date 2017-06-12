package grzegorzjeziorski;

import java.util.ArrayList;
import java.util.Scanner;
import java.util.stream.Stream;

// Link to problem: http://practice.geeksforgeeks.org/problems/ugly-numbers
public class UglyNumbers {

    public static Integer nextUgly(final ArrayList<Integer> uglies) {
        final Integer biggestUgly = uglies.get(uglies.size() - 1);

        return Stream.concat(uglies.stream().map(value -> value * 2), Stream.concat(uglies.stream().map(value ->
                                     value * 3), uglies.stream().map(value -> value * 5))).filter(value ->
                    value > biggestUgly).min(Integer::compare).get();
    }

    public static ArrayList<Integer> computeUglies(final int count) {
        final ArrayList<Integer> result = new ArrayList<>();
        result.add(1);

        int idx = 0;
        while (idx <= count) {
            final Integer nextUgly = nextUgly(result);
            result.add(nextUgly);
            idx++;
        }

        return result;
    }

    public static void main(final String[] args) {
        Scanner in = new Scanner(System.in);
        final ArrayList<Integer> uglies = computeUglies(1000);
        final int N = Integer.parseInt(in.nextLine());
        for (int i = 0; i < N; i++) {
            final int index = Integer.parseInt(in.nextLine());
            System.out.println(uglies.get(index - 1));
        }
    }
}
