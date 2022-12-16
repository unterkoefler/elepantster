CREATE FUNCTION set_updated_at_to_now() RETURNS TRIGGER AS $$
BEGIN
    NEW.updated_at = NOW();
    RETURN NEW;
END;
$$ language PLPGSQL;
CREATE TABLE elephantster_groups (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT now() NOT NULL,
    updated_at TIMESTAMP WITH TIME ZONE DEFAULT now() NOT NULL,
    creator_id UUID NOT NULL,
    name TEXT NOT NULL,
    budget TEXT NOT NULL,
    shared_secret TEXT NOT NULL
);
CREATE INDEX elephantster_groups_created_at_index ON elephantster_groups (created_at);
CREATE TRIGGER update_elephantster_groups_updated_at BEFORE UPDATE ON elephantster_groups FOR EACH ROW EXECUTE FUNCTION set_updated_at_to_now();
CREATE TABLE group_memberships (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT now() NOT NULL,
    updated_at TIMESTAMP WITH TIME ZONE DEFAULT now() NOT NULL,
    user_id UUID DEFAULT ihp_user_id() NOT NULL,
    group_id UUID NOT NULL
);
CREATE INDEX group_memberships_created_at_index ON group_memberships (created_at);
CREATE TRIGGER update_group_memberships_updated_at BEFORE UPDATE ON group_memberships FOR EACH ROW EXECUTE FUNCTION set_updated_at_to_now();
CREATE INDEX group_memberships_user_id_index ON group_memberships (user_id);
ALTER TABLE group_memberships ENABLE ROW LEVEL SECURITY;
CREATE POLICY "Users can manage their group_memberships" ON group_memberships USING (true) WITH CHECK (user_id = ihp_user_id());
CREATE POLICY "Users can manage groups they created" ON elephantster_groups USING (true) WITH CHECK (creator_id = ihp_user_id());
ALTER TABLE elephantster_groups ENABLE ROW LEVEL SECURITY;
CREATE TABLE assignments (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT now() NOT NULL,
    updated_at TIMESTAMP WITH TIME ZONE DEFAULT now() NOT NULL,
    group_id UUID NOT NULL,
    gifter_id UUID NOT NULL,
    giftee_id UUID NOT NULL
);
CREATE INDEX assignments_created_at_index ON assignments (created_at);
CREATE TRIGGER update_assignments_updated_at BEFORE UPDATE ON assignments FOR EACH ROW EXECUTE FUNCTION set_updated_at_to_now();
ALTER TABLE assignments ADD CONSTRAINT assignments_ref_giftee_id FOREIGN KEY (giftee_id) REFERENCES users (id) ON DELETE NO ACTION;
ALTER TABLE assignments ADD CONSTRAINT assignments_ref_gifter_id FOREIGN KEY (gifter_id) REFERENCES users (id) ON DELETE NO ACTION;
ALTER TABLE assignments ADD CONSTRAINT assignments_ref_group_id FOREIGN KEY (group_id) REFERENCES elephantster_groups (id) ON DELETE NO ACTION;
ALTER TABLE elephantster_groups ADD CONSTRAINT elephantster_groups_ref_creator_id FOREIGN KEY (creator_id) REFERENCES users (id) ON DELETE NO ACTION;
ALTER TABLE group_memberships ADD CONSTRAINT group_memberships_ref_group_id FOREIGN KEY (group_id) REFERENCES elephantster_groups (id) ON DELETE NO ACTION;
ALTER TABLE group_memberships ADD CONSTRAINT group_memberships_ref_user_id FOREIGN KEY (user_id) REFERENCES users (id) ON DELETE CASCADE;
