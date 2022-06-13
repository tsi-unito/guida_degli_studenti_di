package com.example.demo;

import java.util.ArrayList;
import java.util.List;
import java.util.Random;
import javax.servlet.ServletException;
import javax.servlet.http.*;
import javax.servlet.annotation.*;

@WebServlet(name = "proverbioservlet", value = "/proverbio-servlet")
public class ProverbioServlet extends HttpServlet {

    private List<String> proverbi;

    @Override
    public void init() throws ServletException {
        super.init();
        proverbi = new ArrayList<>();
        proverbi.add("Chi va con lo zoppo impara a zoppicare");
        proverbi.add("Chi troppo vuole nulla stringe");
        proverbi.add("Se si sputi in aria in faccia ti ritorna");
    }

    public void doGet(HttpServletRequest request, HttpServletResponse response){
        Random r = new Random();
        request.setAttribute("proverbio", proverbi.get(r.nextInt(3)));
    }
}