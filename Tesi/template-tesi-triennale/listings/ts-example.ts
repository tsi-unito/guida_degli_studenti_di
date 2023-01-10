import { Component, OnInit } from '@angular/core';

@Component({
  selector: 'hero-list',
  template: `
    <h2>Hello World</h2>
    <p>This is Hero component list!</p>
  `,
  styleUrls: ['./hero.component.scss'],
  providers:  [ HeroService ]
})
export class HeroListComponent implements OnInit {
  constructor(
    private readonly heroService: HeroService
  ) { }
  
  ngOnInit(): void {
   /* code executed after component rendering */ 
  }
  
  /* All component business logic and data  */
}