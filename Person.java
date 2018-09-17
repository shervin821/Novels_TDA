/**
 * 
 */

/**
 * This class is an auxiliary class for the entity "Person" containing the data for each person extracted from the books. 
 *
 */

package bce;

public class Person {
	private Integer position;
	private String tag;
	private String token;
	
	public Person(Integer position, String tag, String token) {
		super();
		this.position = position;
		this.tag = tag;
		this.token = token;
	}
	
	@Override
    public String toString(){
      return this.getPosition() + "\t" + this.getToken();
    		  }
	
	public Integer getPosition() {
		return position;
	}
	public void setPosition(Integer position) {
		this.position = position;
	}
	public String getTag() {
		return tag;
	}
	public void setTag(String tag) {
		this.tag = tag;
	}
	public String getToken() {
		return token;
	}
	public void setToken(String token) {
		this.token = token;
	}
}
