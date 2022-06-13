package com.example.demo;

import java.sql.*;
import java.util.ArrayList;
import java.util.List;

public class DAO {
    private Connection conn;

    public DAO() throws SQLException {
        DriverManager.registerDriver(new com.mysql.jdbc.Driver());
    }

    public String containsStudente(String nome, String cognome) throws SQLException {
        conn = DriverManager.getConnection("jdbc:mysql://localhost:3306/infostudenti");
        Statement st = conn.createStatement();
        ResultSet rs = st.executeQuery("SELECT * FROM STUDENTI WHERE NOME=" +nome+ " AND COGNOME=" + cognome);
        String result = null;
        if(rs.next()) {
            result = rs.getString("matricola");
        }
        rs.close();
        st.close();
        conn.close();
        return result;
    }

    public List<String> getVotiStudente(String matricola) throws SQLException {
        conn = DriverManager.getConnection("jdbc:mysql://localhost:3306/infostudenti");
        Statement st = conn.createStatement();
        ResultSet rs = st.executeQuery("SELECT VOTO, NOMECORSO FROM CORSI JOIN VOTI ON c.IDCORSO = v.IDCORSO WHERE MATRICOLA="+matricola);
        List<String> result = new ArrayList<>();
        while(rs.next()) {
            result.add(rs.getString("VOTO") + " " + rs.getString("NOMECORSO"));
        }
        rs.close();
        st.close();
        conn.close();
        return result;
    }
}
