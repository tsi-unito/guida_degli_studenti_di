<%@ page import="com.example.demo.ProverbiBean" %>
<%@ page contentType="text/html; charset=UTF-8" pageEncoding="UTF-8" %>
<!DOCTYPE html>
<html>
<head>
    <title>JSP - PROVERBI</title>
</head>
<body>
<h1><%= "CIAO " + session.getAttribute("username") %>
</h1>
<br/>
<p>
    <%= "Il proverbio del giorno " + new ProverbiBean().getProverbio() %>
</p>
</body>
</html>