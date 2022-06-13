package com.example.demo;

import java.util.ArrayList;
import java.util.List;
import java.util.Random;

public class ProverbiBean {
    private String proverbio = "";
    private List<String> proverbi;

    public ProverbiBean() {
        proverbi = new ArrayList<>();
        proverbi.add("CHI VA CON LO ZOPPO IMPARA A ZOPPICARE");
        proverbi.add("CHI TROPPO VUOLE NULLA STRINGE");
        proverbi.add("CHI DORME NON PIGLIA PESCI");
    }
    public String getProverbio() {
        Random r = new Random();
        proverbio = proverbi.get(r.nextInt(3));
        return proverbio;
    }
}
