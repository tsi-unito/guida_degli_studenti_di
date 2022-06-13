package com.example.demo;

import java.io.*;
import java.sql.SQLException;
import java.util.List;
import javax.servlet.http.*;
import javax.servlet.annotation.*;

@WebServlet(name = "helloServlet", value = "/hello-servlet")
public class Informazioni extends HttpServlet {

    public void doGet(HttpServletRequest request, HttpServletResponse response) throws IOException {
        response.setContentType("text/html");

        String stazione = (String) request.getAttribute("stazione");
        PrintWriter out = response.getWriter();
        out.println("<html><body>");
        List<String> result;
        if(stazione != null) {
            try {
                result = DAO.getTreniDiPassaggio(stazione);
                if(!result.isEmpty()) {
                    out.println("<h1> TRENI DI PASSAGGIO IN STAZIONE CON I RITARDI </h1>");
                    for (String el: result) {
                        out.println("<h3>" + el + "</h3>");
                    }
                } else {
                    out.println("<h1>" + "Errore nella stazione" + "</h1>");
                }
            } catch (SQLException throwables) {
                throwables.printStackTrace();
            }
        } else {
            out.println("<h1>" + "Errore nella stazione" + "</h1>");
        }
        out.println("</body></html>");
    }

}