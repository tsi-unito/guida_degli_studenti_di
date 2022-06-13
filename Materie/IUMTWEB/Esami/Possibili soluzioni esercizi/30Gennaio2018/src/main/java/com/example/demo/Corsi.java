package com.example.demo;

import java.io.*;
import java.sql.*;
import javax.servlet.http.*;
import javax.servlet.annotation.*;

@WebServlet(name = "helloServlet", value = "/hello-servlet")
public class Corsi extends HttpServlet {

    public void doGet(HttpServletRequest request, HttpServletResponse response) throws IOException {
        response.setContentType("text/html");
        String CDS = request.getParameter("CDS");
        HttpSession session = request.getSession();
        String username = (String) session.getAttribute("username");
        PrintWriter out = response.getWriter();
        out.println("<html><body>");
        if(username != null) {
            out.println("<h1>" + "CIAO " + username + "</h1>");
        } else {
            out.println("<h1>" + "CIAO UTENTE ANONIMO" + "</h1>");
        }
        if(CDS != null) {
            try {
                DriverManager.registerDriver(new com.mysql.jdbc.Driver());
                Connection conn = DriverManager.getConnection("jdbc:mysql://localhost:3306/DIDATTICA");
                Statement st = conn.createStatement();
                ResultSet rs = st.executeQuery("SELECT CODICE, TITOLO FROM CORSI WHERE CDS="+CDS);
                while(rs.next()) {
                    out.println("<p>" + rs.getString("CODICE") + rs.getString("TITOLO") + "</p>");
                }
                rs.close();
                st.close();
                conn.close();
            } catch (SQLException throwables) {
                throwables.printStackTrace();
            }
        }
        out.println("</body></html>");
    }
}