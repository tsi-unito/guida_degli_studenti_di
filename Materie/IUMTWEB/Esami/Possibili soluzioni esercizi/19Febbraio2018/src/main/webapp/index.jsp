<%@ page import="java.sql.DriverManager" %>
<%@ page import="java.sql.Connection" %>
<%@ page import="java.sql.Statement" %>
<%@ page import="java.sql.SQLException" %>
<%@ page import="com.example.demo.UtenteBean" %>
<%@ page contentType="text/html; charset=UTF-8" pageEncoding="UTF-8" %>
<!DOCTYPE html>
<html>
<head>
    <title>JSP - DATI</title>
</head>
<body>
<h1><%= "CIAO " + request.getParameter("nome") %>
</h1>
<br/>
<%
    UtenteBean utente = new UtenteBean(request.getParameter("account"), request.getParameter("nome"));
    try {
        DriverManager.registerDriver(new com.mysql.jdbc.Driver());
        Connection conn = DriverManager.getConnection("jdbc:mysql://localhost:3306/utenti");
        Statement st = conn.createStatement();
        st.executeUpdate("INSERT INTO UTENTE VALUES (" + utente.getAccount() + ", " + utente.getNome() + ")");
        st.close();
        conn.close();

    } catch (SQLException throwables) {
        throwables.printStackTrace();
    }

%>
</body>
</html>