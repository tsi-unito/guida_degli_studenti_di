package com.example.demo;

import java.io.*;
import java.sql.SQLException;
import java.util.List;
import javax.servlet.http.*;
import javax.servlet.annotation.*;

@WebServlet(name = "informazioni", value = "/informazioni")
public class Informazioni extends HttpServlet {

    public void doGet(HttpServletRequest request, HttpServletResponse response) throws IOException {
        String nome = (String) request.getAttribute("nome");
        String cognome = (String) request.getAttribute("cognome");
        response.setContentType("text/html");
        PrintWriter out = response.getWriter();
        out.println("<html><body>");
        DAO dao;
        String matricola;
        try {
            dao = new DAO();
            if(nome != null && cognome != null) {
                matricola = dao.containsStudente(nome, cognome);
                if(matricola != null) {
                    List<String> voti = dao.getVotiStudente(matricola);
                    out.println("<h1>" + "VOTI DELLO STUDENTE " + nome + " " + cognome + " " + matricola + "</h1>");
                    for (String el : voti) {
                        out.println("<p>" + el + "</p>");
                    }
                } else {
                    out.println("<h1>" + "ERRORE NESSUNO STUDENTE CON QUESTO NOME E COGNOME" + "</h1>");
                }
            } else {
                out.println("<h1>" + "ERRORE NESSUNO STUDENTE CON QUESTO NOME E COGNOME" + "</h1>");
            }
            out.println("</body></html>");
        } catch (SQLException throwables) {
            throwables.printStackTrace();
        }

    }

}