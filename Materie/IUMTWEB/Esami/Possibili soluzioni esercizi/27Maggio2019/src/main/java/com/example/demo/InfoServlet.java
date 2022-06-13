package com.example.demo;

import java.io.*;
import javax.servlet.RequestDispatcher;
import javax.servlet.ServletContext;
import javax.servlet.ServletException;
import javax.servlet.http.*;
import javax.servlet.annotation.*;

@WebServlet(name = "infoServlet", value = "/info-servlet")
public class InfoServlet extends HttpServlet {

    public void doGet(HttpServletRequest request, HttpServletResponse response) throws IOException, ServletException {
        response.setContentType("text/html");
        String operazione = request.getParameter("operazione");
        ServletContext context = getServletContext();
        RequestDispatcher rd = context.getNamedDispatcher("error.jsp");
        PrintWriter out = response.getWriter();
        HttpSession session = request.getSession();
        String nomeUtente = (String) session.getAttribute("nomeUtente");
        String errore = "Richiesta non valida";

        if(operazione != null) {
            if(operazione.equals("dati")) {
                rd = context.getRequestDispatcher("/showData.jsp");
            } else if(operazione.equals("exit")) {
                out.println("<html><body>");
                out.println("<h1>" + "Ciao " + nomeUtente + "</h1>");
                out.println("</body></html>");
                session.invalidate();
                rd = context.getRequestDispatcher("/quit.html");
            } else {
                request.setAttribute("messaggio", errore);
            }
        } else {
            request.setAttribute("messaggio", errore);
        }
        rd.forward(request, response);
    }
}