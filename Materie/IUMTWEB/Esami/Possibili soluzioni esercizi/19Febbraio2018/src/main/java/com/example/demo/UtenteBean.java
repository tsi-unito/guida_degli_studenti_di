package com.example.demo;

public class UtenteBean {
    private String account;
    private String nome;

    public UtenteBean(String account, String nome) {
        this.account = account;
        this.nome = nome;
    }

    public String getAccount() {
        return account;
    }

    public String getNome() {
        return nome;
    }
}
