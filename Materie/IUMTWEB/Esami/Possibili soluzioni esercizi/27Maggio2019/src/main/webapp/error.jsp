<%@ page contentType="text/html; charset=UTF-8" pageEncoding="UTF-8" %>
<!DOCTYPE html>
<html>
<head>
    <title>Errore!</title>
</head>
<body>
<h1><%= "Messaggio di errore " + request.getParameter("messaggio") %>
</h1>
<br/>
</body>
</html>