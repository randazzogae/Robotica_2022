import EDU.gatech.cc.is.util.Vec2;

public class Flag {
    private Vec2 position;
    private int visualNumber;

    public Flag(Vec2 position, int visualNumber) {
        this.position = position;
        this.visualNumber = visualNumber;
    }

    public Vec2 getPosition() {
        return position;
    }

    public void setPosition(Vec2 position) {
        this.position = position;
    }

    public int getVisualNumber() {
        return visualNumber;
    }

    public void setVisualNumber(int visualNumber) {
        this.visualNumber = visualNumber;
    }
}
