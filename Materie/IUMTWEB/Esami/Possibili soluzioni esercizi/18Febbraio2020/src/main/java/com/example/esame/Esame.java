package com.example.esame;

import java.io.*;
import javax.servlet.RequestDispatcher;
import javax.servlet.ServletContext;
import javax.servlet.ServletException;
import javax.servlet.http.*;
import javax.servlet.annotation.*;

@WebServlet(name = "helloServlet", value = "/hello-servlet")
public class Esame extends HttpServlet {


    public void doGet(HttpServletRequest request, HttpServletResponse response) throws IOException, ServletException {
        response.setContentType("text/html");
        String op = (String) request.getAttribute("op");
        ServletContext context = getServletContext();
        RequestDispatcher rd;
        if(op != null) {
            if(op.equals("info")) {
                rd = context.getRequestDispatcher("/gest.html");
                rd.forward(request, response);
            } else if(op.equals("exit")) {
                HttpSession session = request.getSession();
                session.invalidate();
                rd = context.getRequestDispatcher("/bye.html");
                rd.forward(request, response);
            } else {
                rd = context.getRequestDispatcher("/error.jsp");
                request.setAttribute("messaggio", "Errore");
                rd.forward(request, response);
            }
        }
    }
}