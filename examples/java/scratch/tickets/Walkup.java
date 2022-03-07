public class Walkup extends Ticket {
  private double price;

  public Walkup(double price) {
    this.price = price;
  }

  public abstract doulbe getPrice() {
    return price;
  }
}
