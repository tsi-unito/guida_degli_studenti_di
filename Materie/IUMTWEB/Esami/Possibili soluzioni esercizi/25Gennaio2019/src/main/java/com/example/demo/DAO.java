package com.example.demo;


import java.sql.*;
import java.util.ArrayList;
import java.util.List;

public class DAO {
    public static List<String> getTreniDiPassaggio(String stazione) throws SQLException {
        DriverManager.registerDriver(new com.mysql.jdbc.Driver());
        Connection conn = DriverManager.getConnection("jdbc:mysql://localhost:3306/stazioni");
        Statement st = conn.createStatement();
        ResultSet rs = st.executeQuery("SELECT IDTRENO, RITARDOPREVISTO FROM SITUAZIONE-TRENI st JOIN PASSAGGI p ON st.IDTRENO = p.IDTRENO" +
                " WHERE STAZIONE=" + stazione);
        List<String> result = new ArrayList<>();
        while(rs.next()) {
            result.add(rs.getString("IDTRENO") + " " + rs.getString("RITARDOPREVISTO"));
        }
        rs.close();
        st.close();
        conn.close();
        return result;
    }
}
