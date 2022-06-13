<%@ page contentType="text/html; charset=UTF-8" pageEncoding="UTF-8" %>
<!DOCTYPE html>
<html>
<head>
    <title>JSP - PROVERBIO DEL GIORNO</title>
</head>
<body>
<%
    session = request.getSession();
    String username = "";
    if(session.getAttribute("username") != null) {
        username = (String) session.getAttribute("username");
    }
%>
<jsp:include page="/proverbio-servlet"/>

<h1><%= "Hello " + username %> </h1>
<br/>
<h2>
    <%= "Proverbio del giorno: " + request.getAttribute("proverbio")%>
</h2>
</body>
</html>