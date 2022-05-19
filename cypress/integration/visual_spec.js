describe("Draw mw", () => {
  it("visits the page", () => {
    cy.visit("http://localhost:8000/dist/index.html")
    cy.wait(2000)
    cy.contains("Restart").click()
    cy.contains("Pending ticks: 30")
  });

  it("draws something", () => {
    cy.visit("http://localhost:8000/dist/index.html")
    cy.get("canvas")
      .trigger("mousedown", 20, 30)
      .trigger("mousemove", 130, 140)
      .trigger("mouseup")
  })
})
